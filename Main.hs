{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts,
             OverloadedStrings, ViewPatterns #-}
module Main (main) where
import Prelude ()
import Common
import Database
import Diff
import Twitter

import Control.Lens
import Data.Aeson (FromJSON, ToJSON, fromJSON, toJSON)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Web.Twitter.Types.Lens
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JP
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.SQLite as SQL
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Parsec as P

newtype Abort = Abort String deriving (Show, Typeable)
instance Exception Abort

main :: IO ()
main = do
  hSetEncoding stdout utf8
  withDatabase $ \ db ->
    withTwitter db $ do
      myself <- getMyself
      userTracker db myself
      autorestart 1 (userStream (processTimeline db myself))

withTwitter :: (MonadIO m, MonadMask m) =>
               Database -> TwitterM a -> m a
withTwitter db action = do
  (token, secret) <-
    withTransaction db $ \ dbt ->
      ((,) <$> getVariable dbt "token"
           <*> getVariable dbt "token_secret")
  (tw, (token', secret')) <-
    runManagerM (twitterAuth ((,) <$> token <*> secret))
  withTransaction db $ \ dbt -> do
    setVariable dbt "token" token'
    setVariable dbt "token_secret" secret'
  runTwitterM tw action

withDatabase :: (Database -> IO a) -> IO a
withDatabase action = do
  withConnection "c3r.db" $ \ db -> do
    upgradeDatabase db
    action db

-- | Automatically restart if the action fails (after the given delay).
autorestart :: (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
               Double -> m b -> m b
autorestart delay action = do
  result <- tryAny action
  case result of
    Right x -> pure x
    Left  e -> do
      hPutStrLn' stderr (show e)
      sleepSec delay
      autorestart delay action

getUserIdsWithAttr :: MonadIO m => Database -> Text -> JSON.Value -> m [UserId]
getUserIdsWithAttr db attr val = withTransaction db $ \ dbt -> do
  result <-
    sqlExec dbt ["attr" =.. attr, "val" =.. val]
      "SELECT id FROM users WHERE attr = :attr AND val = :val"
  pure $
    case result of
      [rows] -> catMaybes [fromSQLiteValue id | [("id", id)] <- rows]
      _ -> []

loadUserInfo :: MonadIO m => Transaction -> UserId -> m (Map Text JSON.Value)
loadUserInfo dbt userId = do
  result <-
    sqlExec dbt ["id" =.. userId]
      "SELECT attr, val FROM users WHERE id = :id"
  pure . Map.fromList $
    case result of
      [rows] -> catMaybes $
        [ (,) <$> fromSQLiteValue attr
              <*> (fromJSON' =<< fromSQLiteValue val)
        | [("attr", attr), ("val", val)] <- rows]
      _ -> []

saveUserInfo :: MonadIO m =>
                Transaction -> UserId -> Map Text JSON.Value -> m ()
saveUserInfo dbt userId userInfo = do
  sqlExec_ dbt ["id" =.. userId]
    "DELETE FROM users WHERE id = :id"
  for_ (Map.toList userInfo) $ \ (attr, val) -> do
    insertRow dbt "users"
      [ "id" =.. userId
      , "attr" =.. attr
      , "val" =.. val ]

storeUserDiff :: MonadIO m =>
                 Transaction
              -> UserId
              -> Map Text JSON.Value
              -> Map Text JSON.Value
              -> m ()
storeUserDiff dbt userId oldUserInfo userInfo = do
  time <- getCurrentTime
  let (deletes, inserts) = diffMaps oldUserInfo userInfo
  let updates = Map.toList (setToMap SQL.Null deletes) <>
                Map.toList (Map.map toSQLiteValue inserts)
  for_ updates $ \ (attr, val) -> do
    insertRow dbt "user_updates"
      [ "time" =.. time
      , "id" =.. userId
      , "attr" =.. attr
      , "val" =.. val ]

updateUserInfo :: MonadIO m =>
                  Database -> (UserId, Map Text JSON.Value) -> m ()
updateUserInfo db (userId, userInfo) = withTransaction db $ \ dbt -> do
  oldUserInfo <- loadUserInfo dbt userId
  saveUserInfo dbt userId userInfo
  storeUserDiff dbt userId oldUserInfo userInfo

updateUser :: MonadManager r m =>
              Database
           -> JSON.Object
           -> m ()
updateUser db user = do
  followerIds <- Set.fromList <$> getOldFollowerIds db
  updateUserWithFollowerIds followerIds db user

updateUserWithFollowerIds :: MonadManager r m =>
                             Set UserId -> Database -> JSON.Object -> m ()
updateUserWithFollowerIds followerIds db user =
  augmentUserInfo db followerIds user >>=
  traverse_ (updateUserInfo db)

augmentUserInfo :: MonadManager r m =>
                   Database
                -> Set UserId
                -> JSON.Object
                -> m (Maybe (UserId, Map Text JSON.Value))
augmentUserInfo db followerIds user =
  for (user ^. at "id" >>= fromJSON') $ \ userId -> (,) userId <$> do
    hashMapToMap user
      & (`Map.difference` setToMap () (Set.fromList ignoredUserEntries))
      -- note that "following" is already part of the Twitter API
      & ix "follower" .~ JSON.Bool (Set.member userId followerIds)
      & ix "url" (jsonText (dereferenceUrl db))
  where ignoredUserEntries = ["entities", "id", "id_str", "status"]

getOldFriendsIds :: MonadIO m => Database -> m [UserId]
getOldFriendsIds db = getUserIdsWithAttr db "following" (JSON.Bool True)

getOldFollowerIds :: MonadIO m => Database -> m [UserId]
getOldFollowerIds db = getUserIdsWithAttr db "follower" (JSON.Bool True)

userTrackerFrequency :: Num a => a
userTrackerFrequency = 3600

userTrackerInitialDelay :: Num a => a
userTrackerInitialDelay = 60 * 15

userTracker :: MonadTwitter r m => Database -> User -> m ()
userTracker db myself = fork_ . autorestart 60 . forever $ do
  sleepSec userTrackerInitialDelay
  oldFriendIds   <- Set.fromList <$> getOldFriendsIds db
  oldFollowerIds <- Set.fromList <$> getOldFollowerIds db
  friendIds   <- Set.fromList <$> getFriendIds myName
  followerIds <- Set.fromList <$> getFollowerIds myName
  let userIds = Set.singleton myId <>
                friendIds <> followerIds <>
                oldFriendIds <> oldFollowerIds
  for_ (chunkify getUsersById_max (Set.toList userIds)) $ \ userIds' -> do
    users <- getUsersById userIds'
    for_ users $ \ user -> do
      for_ (asJSONObject user) (updateUserWithFollowerIds followerIds db)
  sleepSec (userTrackerFrequency - userTrackerInitialDelay)
  where myName = myself ^. userScreenName
        myId   = myself ^. userId

dereferenceUrl :: MonadManager r m => Database -> Text -> m Text
dereferenceUrl db url | not (isShort url) = pure url
                      | otherwise         = deref url
  where

    isShort url = Text.isPrefixOf "http://t.co/"  url ||
                  Text.isPrefixOf "https://t.co/" url

    getCachedValue key = withTransaction db $ \ dbt -> do
      result <- tryWith (undefined :: DatabaseError) $
                sqlExec dbt ["key" =.. key]
                "SELECT (value) FROM url_cache WHERE key = :key;"
      pure $ case result of
        Right [[[(_, x)]]] -> fromSQLiteValue x
        _                  -> Nothing

    setCachedValue key value = withTransaction db $ \ dbt -> do
      sqlExec_ dbt ["key" =.. key, "value" =.. value]
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

initializeUrlCacheTable :: MonadIO m => Transaction -> m ()
initializeUrlCacheTable dbt =
  createTable dbt "url_cache" ["key TEXT PRIMARY KEY", "value"]

fromJSON' :: FromJSON a => JSON.Value -> Maybe a
fromJSON' x =
  case fromJSON x of
    JSON.Error _ -> Nothing
    JSON.Success x -> Just x

prettyJSON :: ToJSON a => a -> ByteString
prettyJSON =
  ByteStringL.toStrict .
  JP.encodePretty' JP.Config { JP.confIndent = 4, JP.confCompare = compare }

mapJSONObject :: (JSON.Object -> JSON.Object) -> JSON.Value -> JSON.Value
mapJSONObject f (JSON.Object x) = JSON.Object (f x)
mapJSONObject _ z               = z

asJSONObject :: JSON.Value -> Maybe JSON.Object
asJSONObject (JSON.Object x) = Just x
asJSONObject _ = Nothing

jsonText :: Applicative f => (Text -> f Text) -> JSON.Value -> f JSON.Value
jsonText f (JSON.String x) = JSON.String <$> f x
jsonText _ t = pure t

initializeVariableTable :: MonadIO m => Transaction -> m ()
initializeVariableTable dbt =
  createTable dbt "meta" ["key TEXT PRIMARY KEY", "value"]

getVariable :: (MonadIO m, SQLiteValue a) =>
               Transaction -> String -> m (Maybe a)
getVariable dbt key = liftIO $ do
  result <- tryWith (undefined :: DatabaseError) $
            sqlExec dbt ["key" =. SQL.Text key]
            "SELECT value FROM meta WHERE key = :key"
  pure $ case result of
    Right [[[(_, x)]]] -> fromSQLiteValue x
    _                  -> Nothing

setVariable :: (MonadIO m, SQLiteValue a) =>
               Transaction -> String -> a -> m ()
setVariable dbt key value =
  sqlExec_ dbt ["key" =. SQL.Text key, "value" =.. value]
    "INSERT OR REPLACE INTO meta (key, value) VALUES (:key, :value);"

logMessage :: MonadIO m => Database -> String -> m ()
logMessage db message = withTransaction db $ \ dbt -> do
  now <- getCurrentTime
  insertRow dbt "log" ["time" =.. now, "message" =.. message]

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
    [ "time" =.. ds_time r
    , "user_id" =.. ds_userId r
    , "id" =.. ds_id r
    , "text" =.. ds_text r
    , "data" =.. ds_data r
    , "reply_user_id" =.. ds_replyUserId r
    , "reply_status_id" =.. ds_replyStatusId r
    , "rt_user_id" =.. ds_rtUserId r
    , "rt_id" =.. ds_rtId r
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
          (`mapJSONObject` toJSON status) $
          deleteKeysFromHashMap skippedStatusKeys

makeDRtStatus :: UTCTime -> RetweetedStatus -> DStatus
makeDRtStatus time rt =
  (makeDStatus time (rt ^. rsRetweetedStatus))
  { ds_rtUserId = Just (rt ^. rsUser . userId)
  , ds_rtId     = Just (rt ^. rsId)
  , ds_data     = statusDump
  }
  where statusDump =
          (`mapJSONObject` toJSON rt) $
          deleteKeysFromHashMap skippedRtKeys .
          (flip HashMap.adjust "retweeted_status" . mapJSONObject $
           deleteKeysFromHashMap skippedStatusKeys)

deleteKeysFromHashMap :: (Eq k, Hashable k) => [k] -> HashMap k v -> HashMap k v
deleteKeysFromHashMap ks m = foldl' (flip HashMap.delete) m ks

hashMapToMap :: (Ord k, Hashable k) => HashMap k v -> Map k v
hashMapToMap = Map.fromList . HashMap.toList

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
    [ "time" =.. dd_time r
    , "user_id" =.. dd_userId r
    , "status_id" =.. dd_statusId r
    ]

fieldNames_DDelete :: [String]
fieldNames_DDelete = ["time", "user_id", "status_id"]

makeDDelete :: UTCTime -> Delete -> DDelete
makeDDelete time delete =
  DDelete
  time
  (delete ^. delUserId)
  (delete ^. delId)

upgradeDatabase :: MonadIO m => Database -> m ()
upgradeDatabase db = withTransaction db $ \ dbt -> do
  version <- fromMaybe 0 <$> getVariable dbt "version"
  when (version /= currentVersion) $ do
    upgradeFrom dbt (version :: Int)
  where

    currentVersion :: Int
    currentVersion = 1

    upgradeFrom dbt 0 = do
      initializeVariableTable dbt

      -- create various tables
      initializeUrlCacheTable dbt
      createTable dbt "log" ["time", "message"]
      createTable dbt "statuses" fieldNames_DStatus
      createTable dbt "deletes"  fieldNames_DDelete
      createTable dbt "misc_stream_msgs" ["time", "data"]
      createTable dbt "users" ["id", "attr", "val"]
      createTable dbt "user_updates" ["time", "id", "attr", "val"]

      -- upgrade done
      setVariable dbt "version" currentVersion
      putStrLn' "Database initialized."

    upgradeFrom _ v = throwM (Abort ("unknown database version: " <> show v))

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
  putStrLn' "Login successful."
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
sleepSec = threadDelay . round . (1e6 *)

-- todo: rewrite this using JSON.Value instead of Twitter.Types
-- perhaps with the help of lens combinators (is there a way to chain 'at' for nested maps?)
processTimeline :: MonadTwitter r m => Database -> User -> JSON.Value -> m ()
processTimeline db myself (JSON.Object msg) = do
  now <- getCurrentTime
  processStreamMsg db myself msg now
processTimeline _ _ _ =
  hPutStrLn' stderr "processTimeline: invalid stream message"

processStreamMsg :: MonadTwitter r m =>
                    Database -> User -> JSON.Object -> UTCTime -> m ()
processStreamMsg db myself msg now

  -- error
  | Just err <- msg ^. at "error" = do
      hPutStrLn' stderr ("processStreamMsg: error: " <> show err)

  -- retweet
  | Just (JSON.Object rtStatus) <- msg ^. at "retweeted_status" = do
      let status = msg
      fork_ $ do
        for_ (rtStatus ^. at "user" >>= asJSONObject) (updateUser db)
        for_ (status ^. at "user" >>= asJSONObject) (updateUser db)
      for_ (fromJSON' (JSON.Object status)) $ \ status' ->
        withTransaction db $ \ dbt ->
          insertRow dbt "statuses" (makeDRtStatus now status')

  -- status
  | Just _ <- msg ^. at "id" = do
      let status = msg
      fork_ $ do
        for_ (status ^. at "user" >>= asJSONObject) (updateUser db)
      for_ (fromJSON' (JSON.Object status)) $ \ status' -> do
        withTransaction db $ \ dbt ->
          insertRow dbt "statuses" (makeDStatus now status')
        statusHandler db myself status'

  -- delete
  | Just delete <- msg ^. at "delete" = do
      for_ (fromJSON' delete) $ \ delete' ->
        withTransaction db $ \ dbt ->
          insertRow dbt "deletes" (makeDDelete now delete')

  -- friends
  | Just _ <- msg ^. at "friends" = pure ()

  -- other
  | otherwise =
      withTransaction db $ \ dbt ->
        insertRow dbt "misc_stream_msgs"
          [ "time" =.. now
          , "data" =.. JSON.Object msg ]

statusHandler :: MonadTwitter r m => Database -> User -> Status -> m ()
statusHandler db myself status = fromMaybe (pure ()) $ do
  (name, _, message) <- parseStatusText sText
  guard (name == myself ^. userScreenName)
  pure $ runHandlers
    [ if message == ":3"
      then pure . fork_ $ do
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
      pure . fork_ $ do
        factor <- randomRIO (0.01, 1 :: Double)
        let count' = round (fromIntegral count * factor)
            msg    = mconcat (List.replicate count' "arf") <> " :3"
        postReplyR sName msg sId
        logMessage db "replied"

    ]
  where sName = status ^. statusUser . userScreenName
        sText = status ^. statusText
        sId   = status ^. statusId
