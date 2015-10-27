{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             OverloadedStrings, ViewPatterns #-}
module Main (main) where
import Prelude ()
import Common
import Database
import Git
import Twitter

import Control.Lens
import Data.Aeson (ToJSON, toJSON)
import Data.Hashable (Hashable)
import Data.Time (UTCTime, addUTCTime)
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
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Header as HTTP
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
      listLogger db git myName
      taskRunner db
      periodicGitGC git
      autorestart 1 (userStream (processTimeline git db myName))

-- | Automatically restart if the action fails (after the given delay).
autorestart :: (MonadIO m, MonadBaseControl IO m) => Double -> m b -> m b
-- this type signature is not ideal ^
autorestart delay action = do
  result <- tryAny action
  case result of
    Right x -> pure x
    Left  e -> do
      hPutStr' stderr (show e)
      sleepSec delay
      autorestart delay action

periodicGitGC :: (MonadIO m, MonadBaseControl IO m) => Git -> m ()
periodicGitGC git = fork_ . autorestart 60 . forever $ do
  gitGC git
  sleepSec (6 * 3600)

listLogFrequency :: Num a => a
listLogFrequency = 3600

listLogger :: MonadTwitter r m =>
              Database -> Git -> Text -> m ()
listLogger db git myName = fork_ . autorestart 60 . forever $ do
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
      updateUserWithoutCommit db git user
  gitCommit git "Scheduled update"
  gitGC git
  sleepSec listLogFrequency

updateUserWithoutCommit :: MonadManager r m =>
                           Database -> Git -> User -> m ()
updateUserWithoutCommit db git user = do
  user' <- userURL (traverse (dereferenceUrl db)) user
  gitAddFile git ("users/" <> show (user ^. userId)) (prettyJson (strip user'))
  where strip user' = (`mapJSONObject` toJSON user') $
                      deleteKeysFromMap skippedUserKeys

dereferenceUrl :: MonadManager r m => Database -> Text -> m Text
dereferenceUrl db url | not (isShort url) = pure url
                      | otherwise         = deref url
  where

    isShort url = Text.isPrefixOf "http://t.co/"  url ||
                  Text.isPrefixOf "https://t.co/" url

    getCachedValue key = do
      result <- tryWith (__ :: DatabaseError) $
                sqlExec db ["key" =. toSQLiteValue key]
                "SELECT (value) FROM url_cache WHERE key = :key;"
      pure $ case result of
        Right [[[(_, x)]]] -> fromSQLiteValue x
        _                  -> Nothing

    setCachedValue key value = do
      sqlExec_ db ["key" =. toSQLiteValue key, "value" =. toSQLiteValue value]
        "INSERT OR REPLACE INTO url_cache (key, value) VALUES (:key, :value);"

    deref url = do
      result <- getCachedValue url
      case result of
        Just url' -> pure url'
        Nothing   -> do
          result' <- findRedirect url
          case result' of
            Nothing -> pure url
            Just url' -> do
              setCachedValue url url'
              pure url'

findRedirect :: MonadManager r m => Text -> m (Maybe Text)
findRedirect url = do
  mgr <- view manager
  request <- HTTP.parseUrl (Text.unpack url)
  let request' = request { HTTP.redirectCount = 0
                         , HTTP.checkStatus = \_ _ _ -> Nothing }
  response <- tryAny (HTTP.httpLbs request' mgr)
  pure $
    eitherToMaybe . Text.decodeUtf8' =<<
    List.lookup HTTP.hLocation . HTTP.responseHeaders =<<
    eitherToMaybe response

initializeUrlCacheTable :: MonadIO m => Database -> m ()
initializeUrlCacheTable db =
  createTable db "url_cache" ["key TEXT PRIMARY KEY", "value"]

prettyJson :: ToJSON a => a -> ByteString
prettyJson =
  ByteStringL.toStrict .
  JP.encodePretty' JP.Config { JP.confIndent = 4, JP.confCompare = compare }

updateUser :: MonadManager r m => Database -> Git -> User -> m ()
updateUser db git user = do
  updateUserWithoutCommit db git user
  gitCommit git "Streamed update"

ensureDirectoryExist :: MonadIO m => FilePath -> m FilePath
ensureDirectoryExist dir = do
  liftIO (createDirectoryIfMissing True dir)
  pure dir

initializeVariableTable :: MonadIO m => Database -> m ()
initializeVariableTable db =
  createTable db "meta" ["key TEXT PRIMARY KEY", "value"]

getVariable :: (MonadIO m, SQLiteValue a) =>
               Database -> String -> m (Maybe a)
getVariable db key = liftIO $ do
  result <- tryWith (__ :: DatabaseError) $
            sqlExec db ["key" =. SQL.Text key]
            "SELECT value FROM meta WHERE key = :key"
  pure $ case result of
    Right [[[(_, x)]]] -> fromSQLiteValue x
    _                  -> Nothing

setVariable :: (MonadIO m, SQLiteValue a) =>
               Database -> String -> a -> m ()
setVariable db key value =
  sqlExec_ db ["key" =. SQL.Text key, "value" =. toSQLiteValue value]
    "INSERT OR REPLACE INTO meta (key, value) VALUES (:key, :value);"

logMessage :: MonadIO m => Database -> String -> m ()
logMessage db message = do
  now <- getCurrentTime
  void $ insertRow db "log"
    [ "time"    =. toSQLiteValue now
    , "message" =. toSQLiteValue message]

data DStatus =
  DStatus
  { ds_time :: UTCTime
  , ds_userId :: Integer
  , ds_id :: Integer
  , ds_text :: Text
  , ds_data :: JSON.Value
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

-- strip the data that we don't care / change too frequently
skippedUserKeys :: [Text]
skippedUserKeys = ["id", "follow_request_sent", "following", "notifications"]

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

upgradeDatabase :: MonadIO m => Database -> FilePath -> m ()
upgradeDatabase db confDir = liftIO $ do
  version <- fromMaybe 0 <$> getVariable db "version"
  let versionedUpgrades = zip [(0 :: Int) ..] upgrades
  for_ (List.drop version versionedUpgrades) $ \ (v, upgrade) -> do
    upgrade
    setVariable db "version" (v + 1)
    if v == 0
      then putStrLn' "database initialized."
      else putStrLn' ("database upgraded to version " <> show (v + 1) <> ".")

  where

    currentVersion :: Int
    currentVersion = length upgrades

    keysFile = confDir </> "keys"

    upgrades =
      [ do -- version 0
        initializeVariableTable db
        initializeUrlCacheTable db
        createTable db "log" ["time", "message"]
        createTable db "statuses" fieldNames_DStatus
        createTable db "deletes"  fieldNames_DDelete
        createTable db "other_events" ["time", "data"]

        -- migrate keys
        keys <- readFileB keysFile `catchIOError` \ _ -> pure mempty
        case ByteString.lines keys of
          token : secret : _ -> do
            putStrLn' "migrating keys..."
            setVariable db "token"        token
            setVariable db "token_secret" secret
            putStrLn' "keys migrated."
          _ -> pure ()
        removeFile keysFile `catchIOError` \ _ -> pure mempty

        -- version 1
      , createTable db "tasks" ["id INTEGER PRIMARY KEY", "time", "action"]

      ]

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
  pure (newTWInfo cred, cred)

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
  where parser = length <$> some (pTokenS "arf") <* P.optional (pTokenS ":3")

printStatus :: MonadIO m => Status -> m ()
printStatus status = putTextLn' (name <> ": " <> text)
  where name = status ^. statusUser . userScreenName
        text = status ^. statusText

runHandlers :: Monad m => [Maybe (m ())] -> m ()
runHandlers (Just x  : _) = x
runHandlers (Nothing : r) = runHandlers r
runHandlers []            = pure ()

sleepSec :: MonadIO m => Double -> m ()
sleepSec = liftIO . threadDelay . round . (1e6 *)

processTimeline :: MonadTwitter r m =>
                   Git -> Database -> Text -> StreamingAPI -> m ()
processTimeline git db myName streamEvent = do
  now <- getCurrentTime
  case streamEvent of
    SRetweetedStatus rt -> do
      fork_ $ do
        updateUser db git (rt ^. rsRetweetedStatus . statusUser)
        updateUser db git (rt ^. rsUser)
      void $ insertRow db "statuses" (makeDRtStatus now rt)
    SStatus status -> do
      fork_ $ do
        updateUser db git (status ^. statusUser)
      void $ insertRow db "statuses" (makeDStatus now status)
      statusHandler db myName status
    SEvent _ -> do
      void $ insertRow db "other_events"
        ["time" =. toSQLiteValue now,
         "data" =. toSQLiteValue (toJSON streamEvent)]
    SDelete delete -> do
      void $ insertRow db "deletes" (makeDDelete now delete)
    SFriends _ -> do
      pure () -- ignore
    SUnknown event ->
      void $ insertRow db "other_events"
        ["time" =. toSQLiteValue now,
         "data" =. toSQLiteValue event]

data Action
  = Act_ReplyStatus {-screen_name-}Text {-message-}Text {-status_id-}Integer
  deriving (Eq, Ord, Read, Show)

instance SQLiteValue Action

runAction :: MonadTwitter r m => Action -> Database -> m ()
runAction (Act_ReplyStatus sName sMsg sId) db = do
  postReplyR sName sMsg sId
  logMessage db ("replied to " <> show sId)

scheduleTask :: Database -> UTCTime -> Action -> IO ()
scheduleTask db time action =
  void $ insertRow db "tasks"
    [ "time"   =. toSQLiteValue time
    , "action" =. toSQLiteValue action]

data LockMap k a = LockMap (MVar (Map k a))

lockMap_new :: IO (LockMap k a)
lockMap_new = LockMap <$> newMVar Map.empty

lockMap_insert :: (Eq k, Hashable k) => k -> a -> LockMap k a -> IO (Maybe a)
lockMap_insert k v (LockMap lm) =
  modifyMVar lm $ \ m -> pure $
  let m' = Map.insert k v m in
  case Map.lookup k m of
    Just v' -> (m', Just v')
    Nothing -> (m', Nothing)

lockMap_delete :: (Eq k, Hashable k) => k -> LockMap k a -> IO ()
lockMap_delete k (LockMap lm) =
  modifyMVar_ lm (pure . Map.delete k)

taskRunner :: MonadTwitter r m => Database -> m ()
taskRunner db = fork_ $ do
  lm <- liftIO lockMap_new
  forever $ do
    runTasks lm
    sleepSec =<< randomRIO (0.1, 3 :: Double)
  where
    runTasks lm = do
      time <- getCurrentTime
      result <- sqlExec db ["time" =. toSQLiteValue time]
                "SELECT id, action FROM tasks WHERE time <= :time"
      (traverse_ . traverse_) (runTask lm) result

    runTask lm [ (_, fromSQLiteValue -> Just taskId)
               , (_, fromSQLiteValue -> Just action) ] = do
      prevValue <- liftIO (lockMap_insert taskId () lm)
      case prevValue of
        Just () -> pure ()
        Nothing -> fork_ $ do
          runAction action db
          sqlExec_ db ["id" =. toSQLiteValue (taskId :: Integer)]
                   "DELETE FROM tasks WHERE id = :id"
          liftIO (lockMap_delete taskId lm)
    runTask _ _ = pure ()

statusHandler :: MonadTwitter r m => Database -> Text -> Status -> m ()
statusHandler db myName status = fromMaybe (pure ()) $ do
  (name, _, message) <- parseStatusText sText
  guard (name == myName)
  pure $ runHandlers
    [ if message == ":3"
      then Just . fork_ $ do
        delay <- randomRIO (0.01 * 60, 15 * 60 :: Double)
        scheduledTime <- addUTCTime (realToFrac delay) <$> getCurrentTime
        logMessage db ("replying " <> show sId <> " at " <> show scheduledTime)
        liftIO (scheduleTask db scheduledTime (Act_ReplyStatus sName ":3" sId))
      else Nothing

    , do
      count <- parseArfs message
      Just . fork_ $ do
        factor <- randomRIO (0.01, 1 :: Double)
        let count' = round (fromIntegral count * factor)
            msg    = mconcat (List.replicate count' "arf") <> " :3"
        postReplyR sName msg sId
        logMessage db "replied"

    ]
  where sName = status ^. statusUser . userScreenName
        sText = status ^. statusText
        sId   = status ^. statusId
