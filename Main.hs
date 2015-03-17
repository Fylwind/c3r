{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ((<*), (<*>), (*>), (<|>), some, many)
import Control.Exception (catch)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import System.IO.Error (catchIOError)
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

oauth :: OAuth
oauth = twitterOAuth { oauthConsumerKey    = ByteString.pack Keys.consumerKey
                     , oauthConsumerSecret = ByteString.pack Keys.consumerSecret }

authorize :: MonadIO io =>
             Manager
          -> (String -> io String)      -- ^ PIN prompt: @URL -> m PIN@
          -> io Credential
authorize manager getPIN = do
  credential <- OAuth.getTemporaryCredential oauth manager
  let authorizeUrl = takeWhile (/= '&') (OAuth.authorizeUrl oauth credential)
  pin <- getPIN authorizeUrl
  let pin'        = ByteString.pack pin
      credential' = OAuth.insert "oauth_verifier" pin' credential
  OAuth.getAccessToken oauth credential' manager

twitterLogin :: MonadIO io =>
                FilePath
             -> Manager
             -> io TWInfo
twitterLogin accessKeysFile manager = liftIO $ do
  accessKeys <- ByteString.readFile accessKeysFile
                `catchIOError` \ _ -> return mempty
  credential <- case ByteString.lines accessKeys of

    -- use saved keys if they exist
    accessToken : accessSecret : _ ->
      return $ Credential [ ("oauth_token",        accessToken)
                          , ("oauth_token_secret", accessSecret) ]

    -- ask for authorization if keys are absent
    _ -> do
      credential <- authorize manager $ \ url -> do
        putStr $
          "1. Go to this URL (all on one line):\n\n" <>
          url <> "\n\n" <>
          "2. Enter the PIN here:\n\nPIN> "
        hFlush stdout
        getLine
      accessToken  <- lookupCredential credential "oauth_token"
      accessSecret <- lookupCredential credential "oauth_token_secret"
      let accessKeys' = ByteString.unlines [accessToken, accessSecret]
      ByteString.writeFile accessKeysFile accessKeys'
      putStr ("\nAccess keys saved to " <> accessKeysFile <> ".\n\n")
      return credential

  putStr "Login successful.\n\n"
  hFlush stdout
  return (setCredential oauth credential def)
  where lookupCredential (Credential credential) name =
          case List.lookup name credential of
            Just value -> return value
            Nothing    -> ioError (userError "failed to receive access keys")

ensureDirectoryExist :: FilePath -> IO FilePath
ensureDirectoryExist dir = createDirectoryIfMissing True dir >> return dir

processTimeline :: MonadIO io =>
                   Text
                -> TWInfo
                -> StreamingAPI
                -> io ()
processTimeline thisScreenName twitter event = liftIO $ case event of
  SStatus status ->
    let pStatusText = (,,) <$> pScreenName <*> many pScreenName <*> pMessage
        pScreenName = Text.pack <$>
          (P.char '@' *> some (P.alphaNum <|> P.char '_') <* P.spaces)
        pMessage    = many P.anyChar <* P.spaces
        sName       = status ^. statusUser . userScreenName
        sText       = status ^. statusText
        sId         = status ^. statusId
    in case P.parse (P.spaces *> pStatusText) "" sText of
      Right (screenName, otherNames, message)

        | screenName == thisScreenName && message == ":3" ->

          let tweet msg = withManager $ \ manager ->
                let fullMsg = Text.unwords $
                      (("@" <>) <$> sName : otherNames) <> [msg]
                in void . call twitter manager $
                   update fullMsg
                   & inReplyToStatusId ?~ sId
                   & params <>~ [("source", PVString "Wuff")]

              retry n msg = tweet msg `catch` \ err ->
                if twitterErrorCodeIs errStatusDuplicate err && n > 0
                then retry (n - 1 :: Int) (" " <> msg)
                else logTwitterError err

          in do
            Text.putStrLn (sName <> ": " <> sText)
            hFlush stdout
            retry 100 ":3"

      _ -> return ()
  _ -> return ()

errStatusDuplicate :: Int
errStatusDuplicate = 187

twitterErrorCodeIs :: Int -> TwitterError -> Bool
twitterErrorCodeIs code err = case err of
  TwitterErrorResponse _ _
    [TwitterErrorMessage code' _] -> code == code'
  _                               -> False

logTwitterError :: TwitterError -> IO ()
logTwitterError err = do
  case err of
    TwitterErrorResponse _ _ msgs ->
      (Text.hPutStrLn stderr . twitterErrorMessage) `traverse_` msgs
    _ -> hPutStrLn stderr (show err)
  hFlush stderr

main :: IO ()
main = do
  confDir <- ensureDirectoryExist =<< getAppUserDataDirectory "c3r"
  withManager $ \ manager -> do
    twitter <- twitterLogin (confDir </> "keys") manager
    thisUser <- call twitter manager accountVerifyCredentials
    let thisScreenName = thisUser ^. userScreenName
    streamSource <- stream twitter manager userstream
    streamSource $$+- C.mapM_ (processTimeline thisScreenName twitter)
