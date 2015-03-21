{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings #-}
module Twitter
  ( -- * Main module
    module Twitter
    -- * Re-exports
  , TWInfo
  , TwitterError(..)
  , twitterErrorMessage
  ) where
import Control.Exception.Lifted (catch, throwIO)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (($$+-))
import Data.Default (def)
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Conduit
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens hiding (name)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Conduit.List as C
import qualified Data.List as List
import qualified Web.Authenticate.OAuth as OAuth

import qualified Keys

-- | 'Monad' within which HTTP operations may be performed.
type ManagerM a = ReaderT Manager (ResourceT IO) a

-- | 'Monad' within which Twitter operations may be performed.
type TwitterM a = ReaderT (TWInfo, Manager) (ResourceT IO) a

-- | 'Monad's that have a 'Manager' for HTTP connections.
--
--   Note: this is implicitly a subclass of 'MonadIO'.
type MonadManager r m = (HasManager r, MonadReader r m,
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

-- | Run the 'TwitterM' monad.
runTwitterM :: MonadIO m => ManagerM TWInfo -> TwitterM () -> m ()
runTwitterM getTwit action = liftIO . withManager $ \ mgr -> do
  tw <- runReaderT getTwit mgr
  runReaderT action (tw, mgr)

oauth :: OAuth
oauth = twitterOAuth
  { oauthConsumerKey    = ByteString.pack Keys.consumerKey
  , oauthConsumerSecret = ByteString.pack Keys.consumerSecret }

preauthorize :: MonadManager r m => m (Credential, String)
preauthorize = do
  mgr  <- view manager
  cred <- OAuth.getTemporaryCredential oauth mgr
  return (cred, takeWhile (/= '&') (OAuth.authorizeUrl oauth cred))

authorize :: MonadManager r m =>
             Credential -> String -> m (ByteString, ByteString)
authorize cred pin = do
  mgr    <- view manager
  cred   <- OAuth.getAccessToken oauth (combinePinCred pin cred) mgr
  token  <- lookupCred cred "oauth_token"
  secret <- lookupCred cred "oauth_token_secret"
  return (token, secret)
  where combinePinCred pin cred =
          OAuth.insert "oauth_verifier" (ByteString.pack pin) cred
        lookupCred (Credential cred) name =
          case List.lookup name cred of
            Just value -> return value
            Nothing    -> liftIO . ioError . userError $
                          "failed to receive access keys"

newTWInfo :: (ByteString, ByteString) -> TWInfo
newTWInfo (token, secret) = setCredential oauth (Credential cred) def
  where cred = [("oauth_token", token), ("oauth_token_secret", secret)]

-- | Convenience function for extracting the environment.
withTwitter :: MonadTwitter r m => (TWInfo -> Manager -> m a) -> m a
withTwitter action = do
  tw  <- view twitter
  mgr <- view manager
  action tw mgr

getMyName :: MonadTwitter r m => m Text
getMyName = withTwitter $ \ tw mgr -> do
  me <- call tw mgr accountVerifyCredentials
  return (me ^. userScreenName)

userStream :: MonadTwitter r m => (StreamingAPI -> m ()) -> m ()
userStream action = withTwitter $ \ tw mgr -> do
  streamSource <- stream tw mgr userstream
  streamSource $$+- C.mapM_ action

postReply :: MonadTwitter r m => Text -> Integer -> m ()
postReply msg id = withTwitter $ \ tw mgr -> do
  void . call tw mgr $ update msg & inReplyToStatusId ?~ id

-- | Reply the given user, inserting extra spaces if necessary to avoid
--   the "Status duplicate" error
postReply' :: MonadTwitter r m =>
              Text                      -- ^ screen name of recipient
           -> Text                      -- ^ message
           -> Integer                   -- ^ status ID being replied to
           -> m ()
postReply' name msg id =
  postReply msg' id `catch` \ err ->
  if errorCodeIs errStatusDuplicate err && Text.length msg' < maxMsgLen
  then postReply' name (" " <> msg) id
  else throwIO err
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
