{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings #-}
module Main (main) where
import Prelude ()
import Common
import Twitter

import Control.Concurrent (threadDelay)
import Control.Lens
import System.Directory
import System.FilePath
import System.Random (randomRIO)
import Web.Twitter.Types.Lens
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import qualified Text.Parsec as P

main :: IO ()
main = do
  confDir <- ensureDirectoryExist =<< getAppUserDataDirectory "config/c3r"
  runTwitterM (twitterAuth (confDir </> "keys")) $ do
    myName <- getMyName
    userStream (processTimeline myName)

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right x) = Just x

ensureDirectoryExist :: MonadIO io => FilePath -> io FilePath
ensureDirectoryExist dir = do
  liftIO (createDirectoryIfMissing True dir)
  return dir

logTwitterError :: (Applicative m, MonadIO m) => TwitterError -> m ()
logTwitterError err = do
  case err of
    TwitterErrorResponse _ _ msgs
      -> traverse_ (hPutTextLn' stderr . twitterErrorMessage) msgs
    _ -> hPrint' stderr (show err)

twitterAuth :: MonadManager r m => FilePath -> m TWInfo
twitterAuth keysFile = do

  -- read the keys from file
  keys <- readFileB keysFile `catchIOError` \ _ -> return mempty

  -- check whether the keys are stored or absent
  cred <- case ByteString.lines keys of
    token : secret : _ -> return (token, secret)
    _                  -> do
      (cred, url) <- preauthorize
      putStr' ("1. Go to this URL (all on one line):\n\n" <>
               url <> "\n\n" <>
               "2. Enter the PIN here:\n\nPIN> ")
      pin <- getLine
      (token, secret) <- authorize cred pin
      let keys' = ByteString.unlines [token, secret]
      writeFileB keysFile keys'
      putStr' ("\nAccess keys saved to " <> keysFile <> ".\n\n")
      return (token, secret)

  putStr' "Login successful.\n\n"
  return (newTWInfo cred)

-- | Parse the status text to obtain the name of the recipient, names of
--   directly mentioned users, and the message itself.
--
--   Note: mentioned users in the body of the message remain in the message.
parseStatusText :: Text -> Maybe (Text, [Text], Text)
parseStatusText = eitherToMaybe . P.parse (P.spaces *> parser) ""
  where parser   = (,,) <$> name <*> many name <*> message
        message  = Text.pack <$> token (many P.anyChar)
        name     = Text.pack <$> token (P.char '@' *> some nameChar)
        nameChar = P.alphaNum <|> P.char '_'
        token    = (<* P.spaces)

printStatus :: MonadIO m => Status -> m ()
printStatus status = putTextLn' (name <> ": " <> text)
  where name = status ^. statusUser . userScreenName
        text = status ^. statusText

processTimeline :: MonadTwitter r m => Text -> StreamingAPI -> m ()
processTimeline myName event = case event of
  SStatus status
    -> do
      printStatus status
      case parseStatusText sText of
        Just (name, _, message)
          | name == myName && message == ":3"
            -> void . fork $ do
              let coeff = 60 -- seconds
              liftIO $ do
                factor <- randomRIO (0.01, 15 :: Double)
                let delay = coeff * factor
                putStrLn' ("replying in " <> show delay <> " sec")
                threadDelay (round (1e6 * delay))
              postReplyR sName ":3" sId `catch` logTwitterError
              putTextLn' "(replied)"

        _   -> return ()
      where sName = status ^. statusUser . userScreenName
            sText = status ^. statusText
            sId   = status ^. statusId
  _ -> return ()
