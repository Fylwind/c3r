{-# LANGUAGE ConstraintKinds, FlexibleContexts, OverloadedStrings #-}
module Main (main) where
import Prelude ()
import Common
import Twitter

import Control.Lens
import System.Directory
import System.FilePath
import System.Random (Random)
import Web.Twitter.Types.Lens
import qualified System.Random as Random
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

randomRIO :: (MonadIO m, Random a) => (a, a) -> m a
randomRIO = liftIO . Random.randomRIO

randomDouble :: MonadIO m => (Double, Double) -> m Double
randomDouble = randomRIO

randomFiber :: MonadIO m => [[a]] -> m [a]
randomFiber []               = return []
randomFiber (choices : rest) = do
  index <- randomRIO (0, length choices - 1)
  rest' <- randomFiber rest
  return (choices !! index : rest')

runHandlers :: Monad m => [Maybe (m ())] -> m ()
runHandlers (Just x  : _) = x
runHandlers (Nothing : r) = runHandlers r
runHandlers []            = return ()

processTimeline :: MonadTwitter r m => Text -> StreamingAPI -> m ()
processTimeline myName (SStatus status) = do
  printStatus status
  fromMaybe (return ()) $ do
    (name, _, message) <- parseStatusText sText
    guard (name == myName)
    return $ runHandlers
      [ if message == ":3"
        then return . void . fork $ do
          let coeff = 60 -- seconds
          factor <- randomRIO (0.01, 15 :: Double)
          let delay = coeff * factor
          putStrLn' ("---------- replying in " <> show delay <> " sec")
          threadDelay (round (1e6 * delay))
          postReplyR sName ":3" sId
          putTextLn' "---------- replied"
        else mzero

      , do
        count <- parseArfs message
        return . void . fork $ do
          factor <- randomRIO (0.01, 1 :: Double)
          let count' = round (fromIntegral count * factor)
              msg    = mconcat (List.replicate count' "arf") <> " :3"
          postReplyR sName msg sId
          putTextLn' "---------- replied"

      ]

  where sName = status ^. statusUser . userScreenName
        sText = status ^. statusText
        sId   = status ^. statusId
processTimeline _ _ = return ()
