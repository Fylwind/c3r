{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings #-}
module Main (main) where
import Prelude ()
import Common
import Twitter

import Control.Lens
import System.Directory
import System.FilePath
import qualified System.Random as Random
import Web.Twitter.Types.Lens
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Text.Parsec as P

main :: IO ()
main = do
  hSetEncoding stdout utf8
  confDir <- ensureDirectoryExist =<< getAppUserDataDirectory "config/c3r"
  runTwitterM (twitterAuth (confDir </> "keys")) $ do
    myName <- getMyName
    forever (forkWait (userStream (processTimeline myName)))

forkWait :: MonadBaseControl IO m => m a -> m (Either SomeException a)
forkWait action = do
  result <- newEmptyMVar
  _      <- action `forkFinally` putMVar result
  readMVar result

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right x) = Just x

ensureDirectoryExist :: MonadIO io => FilePath -> io FilePath
ensureDirectoryExist dir = do
  liftIO (createDirectoryIfMissing True dir)
  return dir

twitterAuth :: MonadManager r m => FilePath -> m TWInfo
twitterAuth keysFile = do
  putStrLn' "----------------------------------------"

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

  putStrLn' "Login successful.\n----------------------------------------"
  return (newTWInfo cred)

simpleParse :: (P.Stream s Identity t, Show t) =>
               P.Parsec s () a -> s -> Maybe a
simpleParse parser = eitherToMaybe . P.parse (parser <* P.eof) ""

pToken :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m a
pToken x  = P.try (x <* P.spaces)

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
  where parser = length <$> many (pToken (P.string "arf")) <*
                 P.optional (pToken (P.string ":3"))

printStatus :: MonadIO m => Status -> m ()
printStatus status = putTextLn' (name <> ": " <> text)
  where name = status ^. statusUser . userScreenName
        text = status ^. statusText

randomDouble :: MonadIO m => (Double, Double) -> m Double
randomDouble = liftIO . Random.randomRIO

processTimeline :: MonadTwitter r m => Text -> StreamingAPI -> m ()
processTimeline myName event = case event of
  SStatus status
    -> do
      printStatus status
      case parseStatusText sText of
        Just (name, _, message)
          | name == myName ->
            case message of

              ":3" -> void . fork $ do
                let coeff = 60 -- seconds
                factor <- randomDouble (0.01, 15)
                let delay = coeff * factor
                putStrLn' ("---------- replying in " <> show delay <> " sec")
                threadDelay (round (1e6 * delay))
                postReplyR sName ":3" sId
                putTextLn' "---------- replied"

              _ -> case parseArfs message of
                Just count -> void . fork $ do
                      factor <- randomDouble (0.01, 1)
                      let count' = round (fromIntegral count * factor)
                          msg    = mconcat (List.replicate count' "arf") <> " :3"
                      postReplyR sName msg sId
                      putTextLn' "---------- replied"
                _ -> return ()

        _ -> return ()
      where sName = status ^. statusUser . userScreenName
            sText = status ^. statusText
            sId   = status ^. statusId
  _ -> return ()
