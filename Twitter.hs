{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , NoMonomorphismRestriction
  , OverloadedStrings
  , RankNTypes #-}
module Twitter
  ( -- * Main module
    module Twitter
    -- * Re-exports
  , Manager
  , TWInfo
  , TwitterError(..)
  , UserParam(..)
  , friendsList
  , sourceWithCursor
  , twitterErrorMessage
  ) where
import Prelude ()
import Common
import qualified Keys

import Control.Lens
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Aeson (FromJSON)
import Data.Conduit (($$), ($$+-))
import Data.Conduit.List (consume)
import Network.HTTP.Conduit
import Web.Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as P
import Web.Twitter.Types.Lens hiding (name)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Conduit.List as C
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Web.Authenticate.OAuth as OAuth

type ListAPI a c s = (FromJSON s, CursorKey c,
                      P.HasCursorParam (APIRequest a (WithCursor c s)))

getList :: (MonadTwitter r m, ListAPI a c s) =>
           APIRequest a (WithCursor c s) -> m [s]
getList what = do
  tw  <- view twitter
  mgr <- view manager
  sourceWithCursor tw mgr what $$ consume

getFriendIds :: MonadTwitter r m => Text -> m [Integer]
getFriendIds = getList . friendsIds . ScreenNameParam . Text.unpack

getFollowerIds :: MonadTwitter r m => Text -> m [Integer]
getFollowerIds = getList . followersIds . ScreenNameParam . Text.unpack

getUsersById :: MonadTwitter r m => [Integer] -> m [JSON.Value]
getUsersById ids = do
  tw  <- view twitter
  mgr <- view manager
  liftIO (call' tw mgr (usersLookup (UserIdListParam ids)))

getUsersById_max :: Int
getUsersById_max = 100

-- | 'Monad' within which HTTP operations may be performed.
type ManagerM a = ReaderT Manager (ResourceT IO) a

-- | 'Monad' within which Twitter operations may be performed.
type TwitterM a = ReaderT (TWInfo, Manager) (ResourceT IO) a

-- | 'Monad's that have a 'Manager' for HTTP connections.
--
--   Note: this is implicitly a subclass of 'MonadIO'.
type MonadManager r m = (HasManager r, MonadReader r m, MonadMask m,
                         MonadBaseControl IO m, MonadResource m)

-- | 'Monad's that have a 'TWInfo' for HTTP connections.
--
--   Note: this is implicitly a subclass of 'MonadIO'.
type MonadTwitter r m = (HasTwitter r, MonadManager r m)

class HasManager a where
  manager :: Getter a Manager

instance HasManager Manager where
  manager = id

instance HasManager (a, Manager) where
  manager = _2

class HasTwitter a where
  twitter :: Getter a TWInfo

instance HasTwitter TWInfo where
  twitter = id

instance HasTwitter (TWInfo, a) where
  twitter = _1

-- | Run the 'ManagerM' monad.
runManagerM :: MonadIO m => ManagerM a -> m a
runManagerM action = liftIO $ do
  mgr <- newManager tlsManagerSettings
  runResourceT (runReaderT action mgr)

-- | Run the 'TwitterM' monad.
runTwitterM :: MonadIO m => TWInfo -> TwitterM a -> m a
runTwitterM tw action = liftIO $ do
  mgr <- newManager tlsManagerSettings
  runResourceT (runReaderT action (tw, mgr))

oauth :: OAuth
oauth = twitterOAuth
  { oauthConsumerKey    = ByteString.pack Keys.consumerKey
  , oauthConsumerSecret = ByteString.pack Keys.consumerSecret }

preauthorize :: MonadManager r m => m (Credential, String)
preauthorize = do
  mgr  <- view manager
  cred <- OAuth.getTemporaryCredential oauth mgr
  pure (cred, List.takeWhile (/= '&') (OAuth.authorizeUrl oauth cred))

authorize :: MonadManager r m =>
             Credential -> String -> m (ByteString, ByteString)
authorize cred pin = do
  mgr    <- view manager
  cred   <- OAuth.getAccessToken oauth (combinePinCred pin cred) mgr
  token  <- lookupCred cred "oauth_token"
  secret <- lookupCred cred "oauth_token_secret"
  pure (token, secret)
  where combinePinCred pin cred =
          OAuth.insert "oauth_verifier" (ByteString.pack pin) cred
        lookupCred (Credential cred) name =
          case List.lookup name cred of
            Just value -> pure value
            Nothing    -> ioError . userError $
                          "failed to receive access keys"

newTWInfo :: (ByteString, ByteString) -> TWInfo
newTWInfo (token, secret) = setCredential oauth (Credential cred) def
  where cred = [("oauth_token", token), ("oauth_token_secret", secret)]

-- | Convenience function for extracting the environment.
withTWInfo :: MonadTwitter r m => (TWInfo -> Manager -> m a) -> m a
withTWInfo action = do
  tw  <- view twitter
  mgr <- view manager
  action tw mgr

getMyself :: MonadTwitter r m => m User
getMyself = withTWInfo $ \ tw mgr -> do
  liftIO (call tw mgr accountVerifyCredentials)

userStream :: MonadTwitter r m => (JSON.Value -> m ()) -> m ()
userStream action = withTWInfo $ \ tw mgr -> do
  streamSource <- stream' tw mgr userstream
  streamSource $$+- C.mapM_ action

postReply :: MonadTwitter r m => Text -> Integer -> m ()
postReply msg id = withTWInfo $ \ tw mgr -> do
  void (liftIO (call tw mgr (update msg & P.inReplyToStatusId ?~ id)))

-- | Reply the given user, inserting extra spaces if necessary to avoid
--   the "Status duplicate" error
postReplyR :: MonadTwitter r m =>
              Text                      -- ^ screen name of recipient
           -> Text                      -- ^ message
           -> Integer                   -- ^ status ID being replied to
           -> m ()
postReplyR name msg id =
  postReply msg' id `catch` \ err ->
  if errorCodeIs errStatusDuplicate err && Text.length msg' < maxMsgLen
  then postReplyR name (" " <> msg) id
  else throwM err
  where msg' = "@" <> name <> " " <> msg

errStatusDuplicate :: Int
errStatusDuplicate = 187

errorCodeIs :: Int -> TwitterError -> Bool
errorCodeIs code err = case err of
  TwitterErrorResponse _ _ [TwitterErrorMessage code' _]
    -> code == code'
  _ -> False

maxMsgLen :: Int
maxMsgLen = 140
