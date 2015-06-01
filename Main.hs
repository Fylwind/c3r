{-# LANGUAGE ConstraintKinds, FlexibleContexts, OverloadedStrings, ViewPatterns #-}
module Main (main) where
import Prelude ()
import Common
import Database
import Git
import Twitter

import Control.Lens
import Data.Aeson (ToJSON, toJSON)
import Data.Hashable (Hashable)
import Data.Time (UTCTime, getCurrentTime)
import System.Directory
import System.FilePath
import Web.Twitter.Types.Lens
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JP
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Database.SQLite as SQL
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Parsec as P

newtype Abort = Abort String deriving (Show, Typeable)
instance Exception Abort
type Map k v = Map.HashMap k v

main :: IO ()
main = do

  hSetEncoding stdout utf8
  confDir <- ensureDirectoryExist =<< getAppUserDataDirectory "config/c3r"
  git <- gitInit (confDir </> "hist")

  -- initialize database
  withConnection (confDir </> "db") $ \ db -> do
    upgradeDatabase db confDir

    -- connect to Twitter
    token  <- getVariable db "token"
    secret <- getVariable db "token_secret"
    (tw, (token', secret')) <- runManagerM $
      twitterAuth ((,) <$> token <*> secret)
    setVariable db "token" token'
    setVariable db "token_secret" secret'
    runTwitterM tw $ do

      -- start doing stuff
      myName <- getMyName
      listLogger git myName
      autorestart 1 (userStream (processTimeline git db myName))

-- | Automatically restart if the action fails (after the given delay).
autorestart :: (MonadIO m, MonadBaseControl IO m) => Double -> m b -> m b
-- this type signature is not ideal ^
autorestart delay action = do
  result <- tryAny action
  case result of
    Right x -> return x
    Left  e -> do
      hPutStr' stderr (show e)
      sleepSec delay
      autorestart delay action

updateUserWithoutCommit :: MonadIO m => Git -> User -> m ()
updateUserWithoutCommit git user = do
  gitAddFile git ("users/" <> show (user ^. userId)) (prettyJson user)

prettyJson :: ToJSON a => a -> ByteString
prettyJson =
  ByteStringL.toStrict .
  JP.encodePretty' JP.Config { JP.confIndent = 4, JP.confCompare = compare }

updateUser :: MonadIO m => Git -> User -> m ()
updateUser git user = do
  updateUserWithoutCommit git user
  gitCommit git "Streamed update"

listLogFrequency :: Num a => a
listLogFrequency = 3600

listLogger :: MonadTwitter r m => Git -> Text -> m ()
listLogger git myName = void . fork . autorestart 60 . forever $ do
  friendIds'   <- getFriendIds   myName
  followerIds' <- getFollowerIds myName
  let friendIds   = Set.fromList friendIds'
  let followerIds = Set.fromList followerIds'
  let userIds     = friendIds <> followerIds
  gitAddFile git "friends"
    (Text.encodeUtf8 (Text.unlines (showT <$> List.sort friendIds')))
  gitAddFile git "followers"
    (Text.encodeUtf8 (Text.unlines (showT <$> List.sort followerIds')))
  for_ (chunkify getUsersById_max (Set.toList userIds)) $ \ userIds' -> do
    users <- getUsersById userIds'
    for_ users $ \ user -> do
      updateUserWithoutCommit git user
  gitCommit git "Scheduled update"
  sleepSec listLogFrequency

ensureDirectoryExist :: MonadIO m => FilePath -> m FilePath
ensureDirectoryExist dir = do
  liftIO (createDirectoryIfMissing True dir)
  return dir

initializeVariableTable :: MonadIO m => SQLiteHandle -> m ()
initializeVariableTable db =
  createTable db "meta" ["key TEXT PRIMARY KEY", "value"]

getVariable :: (MonadIO m, SQLiteValue a) =>
               SQLiteHandle -> String -> m (Maybe a)
getVariable db key = liftIO $ do
  result <- tryWith (undefined :: DatabaseError) $
            sqlExec db ["key" =. SQL.Text key]
            "SELECT value FROM meta WHERE key = :key"
  case result of
    Right [[[("value", x)]]] -> return (fromSQLiteValue x)
    _                        -> return Nothing

setVariable :: (MonadIO m, SQLiteValue a) =>
               SQLiteHandle -> String -> a -> m ()
setVariable db key value =
  sqlExec_ db ["key" =. SQL.Text key, "value" =. toSQLiteValue value]
    "INSERT OR REPLACE INTO meta (key, value) VALUES (:key, :value);"

logMessage :: MonadIO m => SQLiteHandle -> String -> m ()
logMessage db message = do
  now <- liftIO getCurrentTime
  insertRow db "meta" ["time" =. toSQLiteValue now,
                       "message" =. toSQLiteValue message]

data DStatus =
  DStatus
  { ds_time :: UTCTime
  , ds_userId :: Integer
  , ds_id :: Integer
  , ds_text :: Text
  , ds_data :: ByteString
  , ds_replyUserId :: Maybe Integer
  , ds_replyStatusId :: Maybe Integer
  , ds_rtUserId :: Maybe Integer
  , ds_rtId :: Maybe Integer
  }

instance SQLiteRecord DStatus where
  fromSQLiteRecord (Map.fromList -> r) =
    DStatus <$>
    (fromSQLiteValue =<< Map.lookup "time" r) <*>
    (fromSQLiteValue =<< Map.lookup "user_id" r) <*>
    (fromSQLiteValue =<< Map.lookup "id" r) <*>
    (fromSQLiteValue =<< Map.lookup "text" r) <*>
    (fromSQLiteValue =<< Map.lookup "data" r) <*>
    (fromSQLiteValue =<< Map.lookup "reply_user_id" r) <*>
    (fromSQLiteValue =<< Map.lookup "reply_status_id" r) <*>
    (fromSQLiteValue =<< Map.lookup "rt_user_id" r) <*>
    (fromSQLiteValue =<< Map.lookup "rt_id" r)
  toSQLiteRecord r =
    [ "time" =. toSQLiteValue (ds_time r)
    , "user_id" =. toSQLiteValue (ds_userId r)
    , "id" =. toSQLiteValue (ds_id r)
    , "text" =. toSQLiteValue (ds_text r)
    , "data" =. toSQLiteValue (ds_data r)
    , "reply_user_id" =. toSQLiteValue (ds_replyUserId r)
    , "reply_status_id" =. toSQLiteValue (ds_replyStatusId r)
    , "rt_user_id" =. toSQLiteValue (ds_rtUserId r)
    , "rt_id" =. toSQLiteValue (ds_rtId r)
    ]

fieldNames_DStatus :: [String]
fieldNames_DStatus = ["time", "user_id", "id", "text", "data",
                      "reply_user_id", "reply_status_id",
                      "rt_user_id", "rt_id"]

-- strip the data that we are storing explicitly
skippedStatusKeys :: [Text]
skippedStatusKeys = ["user", "id", "text",
                     "in_reply_to_user_id",
                     "in_reply_to_status_id"]

skippedRtKeys :: [Text]
skippedRtKeys = ["user", "id"]

makeDStatus :: UTCTime -> Status -> DStatus
makeDStatus time status =
  DStatus
  time
  (status ^. statusUser . userId)
  (status ^. statusId)
  (status ^. statusText)
  statusDump
  (status ^. statusInReplyToUserId)
  (status ^. statusInReplyToStatusId)
  Nothing
  Nothing
  where statusDump =
          ByteStringL.toStrict . JSON.encode .
          (`mapJSONObject` toJSON status) $
          deleteKeysFromMap skippedStatusKeys

mapJSONObject :: (JSON.Object -> JSON.Object) -> JSON.Value -> JSON.Value
mapJSONObject f (JSON.Object x) = JSON.Object (f x)
mapJSONObject _ z               = z

makeDRtStatus :: UTCTime -> RetweetedStatus -> DStatus
makeDRtStatus time rt =
  (makeDStatus time (rt ^. rsRetweetedStatus))
  { ds_rtUserId = Just (rt ^. rsUser . userId)
  , ds_rtId     = Just (rt ^. rsId)
  , ds_data     = statusDump
  }
  where statusDump =
          ByteStringL.toStrict . JSON.encode .
          (`mapJSONObject` toJSON rt) $
          deleteKeysFromMap skippedRtKeys .
          (flip Map.adjust "retweeted_status" . mapJSONObject $
           deleteKeysFromMap skippedStatusKeys)

deleteKeysFromMap :: (Eq k, Hashable k) => [k] -> Map k v -> Map k v
deleteKeysFromMap ks m = foldl' (flip Map.delete) m ks

data DDelete =
  DDelete
  { dd_time :: UTCTime
  , dd_userId :: Integer
  , dd_statusId :: Integer
  }

instance SQLiteRecord DDelete where
  fromSQLiteRecord (Map.fromList -> r) =
    DDelete <$>
    (fromSQLiteValue =<< Map.lookup "time" r) <*>
    (fromSQLiteValue =<< Map.lookup "user_id" r) <*>
    (fromSQLiteValue =<< Map.lookup "status_id" r)
  toSQLiteRecord r =
    [ "time" =. toSQLiteValue (dd_time r)
    , "user_id" =. toSQLiteValue (dd_userId r)
    , "status_id" =. toSQLiteValue (dd_statusId r)
    ]

fieldNames_DDelete :: [String]
fieldNames_DDelete = ["time", "user_id", "status_id"]

makeDDelete :: UTCTime -> Delete -> DDelete
makeDDelete time delete =
  DDelete
  time
  (delete ^. delUserId)
  (delete ^. delId)

upgradeDatabase :: MonadIO m => SQLiteHandle -> FilePath -> m ()
upgradeDatabase db confDir = liftIO $ do
  version <- fromMaybe 0 <$> getVariable db "version"
  when (version /= currentVersion) $ do
    upgradeFrom (version :: Int)
  where

    currentVersion :: Int
    currentVersion = 1

    upgradeFrom 0 = do
      initializeVariableTable db

      -- create various tables
      createTable db "log" ["time", "message"]
      createTable db "statuses" fieldNames_DStatus
      createTable db "deletes"  fieldNames_DDelete
      createTable db "other_events" ["time", "data"]

      -- migrate keys
      keys <- readFileB keysFile `catchIOError` \ _ -> return mempty
      case ByteString.lines keys of
        token : secret : _ -> do
          putStrLn' "migrating keys..."
          setVariable db "token"        token
          setVariable db "token_secret" secret
          putStrLn' "keys migrated."
        _ -> return ()

      -- upgrade done
      setVariable db "version" currentVersion
      putStrLn' "database initialized."

      -- cleanup
      removeFile keysFile `catchIOError` \ _ -> return mempty

      where keysFile = confDir </> "keys"

    upgradeFrom v = throwIO (Abort ("unknown database version: " <> show v))

twitterAuth :: MonadManager r m =>
               Maybe (ByteString, ByteString)
            -> m (TWInfo, (ByteString, ByteString))
twitterAuth Nothing = do
  (cred, url) <- preauthorize
  putStr' ("1. Go to this URL (all on one line):\n\n" <> url <> "\n\n" <>
           "2. Enter the PIN here:\n\nPIN> ")
  pin <- getLine
  cred <- authorize cred pin
  twitterAuth (Just cred)
twitterAuth (Just cred) = do
  putStrLn' "Login successful.\n"
  return (newTWInfo cred, cred)

simpleParse :: (P.Stream s Identity t, Show t) =>
               P.Parsec s () a -> s -> Maybe a
simpleParse parser = eitherToMaybe . P.parse (parser <* P.eof) ""

pToken :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m a
pToken x = P.try (x <* P.spaces)

pTokenS :: P.Stream s m Char => String -> P.ParsecT s u m Text
pTokenS = (Text.pack <$>) . pToken . P.string

pWord :: P.Stream s m Char => P.ParsecT s u m Text
pWord = Text.pack <$> some P.alphaNum

-- | Parse the status text to obtain the name of the recipient, names of
--   directly mentioned users, and the message itself.
--
--   Note: mentioned users in the body of the message remain in the message.
parseStatusText :: Text -> Maybe (Text, [Text], Text)
parseStatusText = simpleParse (P.spaces *> parser)
  where parser   = (,,) <$> name <*> many name <*> message
        message  = Text.pack <$> pToken (many P.anyChar)
        name     = Text.pack <$> pToken (P.char '@' *> some nameChar)
        nameChar = P.alphaNum <|> P.char '_'

parseArfs :: Text -> Maybe Int
parseArfs = simpleParse (P.spaces *> parser)
  where parser = length <$> many (pTokenS "arf") <* P.optional (pTokenS ":3")

printStatus :: MonadIO m => Status -> m ()
printStatus status = putTextLn' (name <> ": " <> text)
  where name = status ^. statusUser . userScreenName
        text = status ^. statusText

runHandlers :: Monad m => [Maybe (m ())] -> m ()
runHandlers (Just x  : _) = x
runHandlers (Nothing : r) = runHandlers r
runHandlers []            = return ()

sleepSec :: MonadIO m => Double -> m ()
sleepSec = liftIO . threadDelay . round . (1e6 *)

processTimeline :: MonadTwitter r m =>
                   Git -> SQLiteHandle -> Text -> StreamingAPI -> m ()
processTimeline git db myName streamEvent = do
  now <- liftIO getCurrentTime
  case streamEvent of
    SRetweetedStatus rt -> do
      void . fork $ do
        updateUser git (rt ^. rsRetweetedStatus . statusUser)
        updateUser git (rt ^. rsUser)
      insertRow db "statuses" (makeDRtStatus now rt)
    SStatus status -> do
      void . fork $ do
        updateUser git (status ^. statusUser)
      insertRow db "statuses" (makeDStatus now status)
      statusHandler db myName status
    SEvent event -> do
      insertRow db "other_events"
        ["time" =. toSQLiteValue now,
         "data" =. toSQLiteValue (show event)]
    SDelete delete -> do
      insertRow db "deletes" (makeDDelete now delete)
    SFriends _ -> do
      return () -- ignore
    SUnknown event ->
      insertRow db "other_events"
        ["time" =. toSQLiteValue now,
         "data" =. toSQLiteValue event]

statusHandler :: MonadTwitter r m => SQLiteHandle -> Text -> Status -> m ()
statusHandler db myName status = fromMaybe (return ()) $ do
  (name, _, message) <- parseStatusText sText
  guard (name == myName)
  return $ runHandlers
    [ if message == ":3"
      then return . void . fork $ do
        let coeff = 60 -- seconds
        factor <- randomRIO (0.01, 15 :: Double)
        let delay = coeff * factor
        logMessage db ("replying " <> show sId <>
                       " in " <> show delay <> " sec")
        sleepSec delay
        postReplyR sName ":3" sId
        logMessage db ("replied to " <> show sId)
      else mzero

    , do
      count <- parseArfs message
      return . void . fork $ do
        factor <- randomRIO (0.01, 1 :: Double)
        let count' = round (fromIntegral count * factor)
            msg    = mconcat (List.replicate count' "arf") <> " :3"
        postReplyR sName msg sId
        logMessage db "replied"

    ]
  where sName = status ^. statusUser . userScreenName
        sText = status ^. statusText
        sId   = status ^. statusId
