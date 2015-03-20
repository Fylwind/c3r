{-# LANGUAGE FlexibleContexts
           , NoMonomorphismRestriction
           , OverloadedStrings
           , ScopedTypeVariables #-}
module Main (main) where
import Control.Applicative ((<*), (<*>), (*>), (<|>), some, many)
import Control.Lens
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Reader.Class
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Functor ((<$>), void)
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Network.HTTP.Conduit
import System.Directory
import System.FilePath
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Control.Monad.Catch (MonadCatch, catch, catchIOError)
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens hiding (name)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Conduit.List as C
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.Parsec as P
import qualified Web.Authenticate.OAuth as OAuth
import qualified Keys

class HasManager a where
  manager :: IndexPreservingGetter a Manager

oauth :: OAuth
oauth = twitterOAuth { oauthConsumerKey    = ByteString.pack Keys.consumerKey
                     , oauthConsumerSecret = ByteString.pack Keys.consumerSecret }

authorize :: (MonadIO io, MonadReader env io, HasManager env) =>
             (String -> io String)      -- ^ PIN prompt: @URL -> m PIN@
          -> io Credential
authorize getPIN = do
  mgr  <- (^. manager) <$> ask
  cred <- OAuth.getTemporaryCredential oauth mgr
  pin  <- getPIN (makeAuthUrl cred)
  OAuth.getAccessToken oauth (combinePinCred pin cred) mgr
  where

    (<$>) = liftM                       -- for backward compatibility only

    makeAuthUrl cred =
      takeWhile (/= '&') (OAuth.authorizeUrl oauth cred)

    combinePinCred pin cred =
      OAuth.insert "oauth_verifier" (ByteString.pack pin) cred

twitterLogin :: (MonadIO io, MonadReader env io, HasManager env) =>
                FilePath
             -> io TWInfo
twitterLogin keysFile = do

  -- read the keys from file
  keys <- liftIO $ ByteString.readFile keysFile
                   `catchIOError` \ _ -> return mempty

  cred <- case ByteString.lines keys of
    -- if keys are stored
    token : secret : _ -> return (makeAuth token secret)
    -- if keys are absent
    _                  -> requestAuth

  liftIO $ do
    putStr "Login successful.\n\n"
    hFlush stdout
  return (setCredential oauth cred def)

  where

    requestAuth = do
      cred <- authorize $ \ url -> liftIO $ do
        putStr ("1. Go to this URL (all on one line):\n\n" <>
                url <> "\n\n" <>
                "2. Enter the PIN here:\n\nPIN> ")
        hFlush stdout
        getLine
      accessToken  <- lookupCred cred "oauth_token"
      accessSecret <- lookupCred cred "oauth_token_secret"
      let keys' = ByteString.unlines [accessToken, accessSecret]
      liftIO $ do
        ByteString.writeFile keysFile keys'
        putStr ("\nAccess keys saved to " <> keysFile <> ".\n\n")
      return cred

    makeAuth token secret =
      Credential [("oauth_token", token), ("oauth_token_secret", secret)]

    lookupCred (Credential cred) name =
      case List.lookup name cred of
        Just value -> return value
        Nothing    -> liftIO . ioError . userError $
                      "failed to receive access keys"

ensureDirectoryExist :: MonadIO io => FilePath -> io FilePath
ensureDirectoryExist dir = do
  liftIO $ createDirectoryIfMissing True dir
  return dir

processTimeline :: (MonadIO io, MonadCatch io, MonadResource io,
                    MonadReader env io, HasManager env) =>
                   Text
                -> TWInfo
                -> StreamingAPI
                -> io ()
processTimeline thisScreenName twitter event = case event of
  SStatus status ->
    let pStatusText = (,,) <$> pScreenName <*> many pScreenName <*> pMessage
        pScreenName = Text.pack <$>
          (P.char '@' *> some (P.alphaNum <|> P.char '_') <* P.spaces)
        pMessage    = many P.anyChar <* P.spaces
        sName       = status ^. statusUser . userScreenName
        sText       = status ^. statusText
        sId         = status ^. statusId
    in do
      liftIO $ do
        Text.putStrLn (sName <> ": " <> sText)
        hFlush stdout
      case P.parse (P.spaces *> pStatusText) "" sText of
        Right (screenName, otherNames, message)

          | screenName == thisScreenName && message == ":3" ->

            let tweet msg = do
                  mgr <- (^. manager) <$> ask
                  let fullMsg = Text.unwords $
                        (("@" <>) <$> sName : otherNames) <> [msg]
                    in void . call twitter mgr $
                       update fullMsg
                       & inReplyToStatusId ?~ sId

                retry n msg =
                  tweet msg `catch` \ err ->
                  if twitterErrorCodeIs errStatusDuplicate err && n > 0
                  then retry (n - 1 :: Int) (" " <> msg)
                  else logTwitterError err

            in do
              retry 100 ":3"
              liftIO $ do
                putStrLn "(replied)"
                hFlush stdout

        _ -> return ()
  _ -> return ()

errStatusDuplicate :: Int
errStatusDuplicate = 187

twitterErrorCodeIs :: Int -> TwitterError -> Bool
twitterErrorCodeIs code err = case err of
  TwitterErrorResponse _ _
    [TwitterErrorMessage code' _] -> code == code'
  _                               -> False

logTwitterError :: MonadIO io => TwitterError -> io ()
logTwitterError err = liftIO $ do
  case err of
    TwitterErrorResponse _ _ msgs ->
      (Text.hPutStrLn stderr . twitterErrorMessage) `traverse_` msgs
    _ -> hPutStrLn stderr (show err)
  hFlush stderr

data Env = Env { _manager :: Manager }

instance HasManager Env where
  manager = to _manager

main :: IO ()
main = do
  confDir <- ensureDirectoryExist =<< liftIO (getAppUserDataDirectory "c3r")
  withManager $ \ mgr -> (`runReaderT` Env mgr) $ do
    let keysFile = confDir </> "keys"
    twitter <- twitterLogin keysFile
    thisUser <- call twitter mgr accountVerifyCredentials
    let thisScreenName = thisUser ^. userScreenName
    streamSource <- stream twitter mgr userstream
    streamSource $$+- C.mapM_ (processTimeline thisScreenName twitter)
