{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
module Common
  ( module Common
  , module Calico.Base
  , module Calico.MonadIOControl
  , module Calico.ByteString.MonadIO
  , module Calico.Text.MonadIO
  ) where
import Prelude ()
import Calico.Base
import Calico.MonadIOControl
import Calico.ByteString.MonadIO
import Calico.Text.MonadIO
import Data.Time (UTCTime)
import System.Directory
import System.Random (Random)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.Random as Random

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right x) = Just x

__ :: a
__ = error "__: dummy value is not meant to be used"

chunkify :: Int -> [a] -> [[a]]
chunkify _ []            = []
chunkify k l | k > 0     = chunk : chunkify k rest
             | otherwise = error "chunkify: first argument must be nonnegative"
  where (chunk, rest) = List.splitAt k l

showT :: Show a => a -> Text
showT = Text.pack . show

throwIfLeft :: (MonadIO m, Exception e) => (b -> e) -> Either b a -> m a
throwIfLeft f (Left  e) = liftIO (throwIO (f e))
throwIfLeft _ (Right x) = pure x

throwIfJust :: (MonadIO m, Exception e) => (b -> e) -> Maybe b -> m ()
throwIfJust f (Just  e) = liftIO (throwIO (f e))
throwIfJust _ Nothing   = pure ()

tryAny :: MonadBaseControl IO m => m a -> m (Either SomeException a)
tryAny action = do
  result <- newEmptyMVar
  mask $ \ unmask -> do
    thread <- fork (try (unmask action) >>= putMVar result)
    unmask (readMVar result) `onException` killThread thread

tryWith :: (MonadBaseControl IO m, Exception e) => e -> m a -> m (Either e a)
tryWith _ = try

randomRIO :: (MonadIO m, Random a) => (a, a) -> m a
randomRIO = liftIO . Random.randomRIO

randomDouble :: MonadIO m => (Double, Double) -> m Double
randomDouble = randomRIO

randomFiber :: MonadIO m => [[a]] -> m [a]
randomFiber []               = pure []
randomFiber (choices : rest) = do
  index <- randomRIO (0, length choices - 1)
  rest' <- randomFiber rest
  pure (choices !! index : rest')

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

-- | Perform the given action with a different working directory.
withDir :: FilePath -> IO a -> IO a
withDir dir action = do
  dir' <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir) (setCurrentDirectory dir') action

_s :: String -> String
_s = id

(=.) :: String -> a -> (String, a)
(=.) = (,)

traceS :: Show a => a -> a
traceS x = traceShow x x
