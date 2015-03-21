{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , NoMonomorphismRestriction
  , OverloadedStrings #-}
module Main (main) where
import Control.Applicative ((<*), (<*>), (*>), (<|>), some, many)
import Control.Lens
import Control.Monad.Catch (catch, catchIOError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Functor ((<$>))
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import System.Directory
import System.FilePath
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Web.Twitter.Types.Lens
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.Parsec as P

import Twitter

ensureDirectoryExist :: MonadIO io => FilePath -> io FilePath
ensureDirectoryExist dir = do
  liftIO (createDirectoryIfMissing True dir)
  return dir

twitterAuth :: MonadManager r m => FilePath -> m TWInfo
twitterAuth keysFile = do

  -- read the keys from file
  keys <- liftIO $
    ByteString.readFile keysFile
    `catchIOError` \ _ -> return mempty

  -- check whether the keys are stored or absent
  cred <- case ByteString.lines keys of
    token : secret : _ -> return (token, secret)
    _                  -> do
      (cred, url) <- preauthorize
      pin <- liftIO $ do
        putStr ("1. Go to this URL (all on one line):\n\n" <>
                url <> "\n\n" <>
                "2. Enter the PIN here:\n\nPIN> ")
        hFlush stdout
        getLine
      (token, secret) <- authorize cred pin
      let keys' = ByteString.unlines [token, secret]
      liftIO $ do
        ByteString.writeFile keysFile keys'
        putStr ("\nAccess keys saved to " <> keysFile <> ".\n\n")
      return (token, secret)

  liftIO $ do
    putStr "Login successful.\n\n"
    hFlush stdout
  return (newTWInfo cred)

processTimeline :: MonadTwitter r m => Text -> StreamingAPI -> m ()
processTimeline thisScreenName event = case event of
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

            let tweet msg =
                  let fullMsg = Text.unwords $
                        (("@" <>) <$> sName : otherNames) <> [msg]
                  in postReply fullMsg sId

                retry :: MonadTwitter r m => Int -> Text -> m ()
                retry n msg =
                  tweet msg `catch` \ err ->
                  if errorCodeIs errStatusDuplicate err && n > 0
                  then retry (n - 1) (" " <> msg)
                  else logTwitterError err

            in do
              retry 100 ":3"
              liftIO $ do
                putStrLn "(replied)"
                hFlush stdout

        _ -> return ()
  _ -> return ()

logTwitterError :: MonadIO m => TwitterError -> m ()
logTwitterError err = liftIO $ do
  case err of
    TwitterErrorResponse _ _ msgs
      -> traverse_ (Text.hPutStrLn stderr . twitterErrorMessage) msgs
    _ -> hPutStrLn stderr (show err)
  hFlush stderr

main :: IO ()
main = do
  confDir <- ensureDirectoryExist =<< getAppUserDataDirectory "config/c3r"
  runTwitterM (twitterAuth (confDir </> "keys")) $ do
    myName <- getMyName
    userStream (processTimeline myName)
