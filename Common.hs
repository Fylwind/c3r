{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
module Common
  ( module Common
  , module Calico.Base
  , module Calico.MonadException
  , module Calico.MonadIOControl
  , module Calico.ByteString.MonadIO
  , module Calico.Text.MonadIO
  , UTCTime
  , diffUTCTime
  ) where
import Prelude ()
import Calico.Base
import Calico.MonadException
import Calico.MonadIOControl
import Calico.ByteString.MonadIO
import Calico.Text.MonadIO
import Data.Time (UTCTime, diffUTCTime)
import System.Directory
import System.Random (Random)
import qualified System.Timeout
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.Random as Random

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right x) = Just x

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

chunkify :: Int -> [a] -> [[a]]
chunkify _ []            = []
chunkify k l | k > 0     = chunk : chunkify k rest
             | otherwise = error "chunkify: first argument must be nonnegative"
  where (chunk, rest) = List.splitAt k l

showT :: Show a => a -> Text
showT = Text.pack . show

sleepSec :: MonadIO m => Double -> m ()
sleepSec t = threadDelay (round (1e6 * t))

timeoutSec :: MonadIO m => Double -> IO a -> m (Maybe a)
timeoutSec t m = liftIO (System.Timeout.timeout (round (1e6 * t)) m)

throwIfLeft :: (MonadThrow m, Exception e) => (b -> e) -> Either b a -> m a
throwIfLeft f (Left  e) = throwM (f e)
throwIfLeft _ (Right x) = pure x

throwIfJust :: (MonadThrow m, Exception e) => (b -> e) -> Maybe b -> m ()
throwIfJust f (Just  e) = throwM (f e)
throwIfJust _ Nothing   = pure ()

tryAny :: (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
          m a -> m (Either SomeException a)
tryAny action = do
  result <- newEmptyMVar
  mask $ \ unmask -> do
    thread <- fork (try (unmask action) >>= putMVar result)
    unmask (readMVar result) `onException` killThread thread

tryWith :: (MonadCatch m, Exception e) => e -> m a -> m (Either e a)
tryWith _ = try

waitEither :: (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
              m a -> m a -> m (Either SomeException a)
waitEither action1 action2 = do
  result <- newEmptyMVar
  mask $ \ unmask -> do
    thread1 <- fork (try (unmask action1) >>= putMVar result)
    thread2 <- fork (try (unmask action2) >>= putMVar result)
    unmask (readMVar result) `finally` do
      killThread thread1
      killThread thread2

newtype Watchdog = Watchdog (MVar ())

newWatchdog :: MonadIO m => m Watchdog
newWatchdog = Watchdog <$> newEmptyMVar

withWatchdog :: (MonadIO m, MonadMask m, MonadBaseControl IO m) =>
                Double
             -> (Watchdog -> m a)
             -> m (Either SomeException (Maybe a))
withWatchdog interval action = do
  watchdog <- newWatchdog
  waitEither
    (Nothing <$ watchdogThread watchdog interval)
    (Just <$> action watchdog)

watchdogThread :: MonadIO m => Watchdog -> Double -> m ()
watchdogThread watchdog@(Watchdog var) interval = do
  result <- timeoutSec interval (takeMVar var)
  case result of
    Nothing -> pure ()
    Just () -> watchdogThread watchdog interval

kickWatchdog :: MonadIO m => Watchdog -> m ()
kickWatchdog (Watchdog var) = void (tryPutMVar var ())

randomIO :: (MonadIO m, Random a) => m a
randomIO = liftIO Random.randomIO

randomRIO :: (MonadIO m, Random a) => (a, a) -> m a
randomRIO = liftIO . Random.randomRIO

randomDouble :: MonadIO m => (Double, Double) -> m Double
randomDouble = randomRIO

randomExponential :: (Floating b, MonadIO f, Random b) => b -> f b
randomExponential mean =
  (\ x -> (-log x) * mean) <$> randomIO

randomFiber :: MonadIO m => [[a]] -> m [a]
randomFiber []               = pure []
randomFiber (choices : rest) = do
  index <- randomRIO (0, length choices - 1)
  rest' <- randomFiber rest
  pure (choices !! index : rest')

-- | A sigmoid function defined by @('tanh' x + 1) / 2@.
sigmoidTanh :: Floating a => a -> a
sigmoidTanh x = 0.5 * (tanh x + 1)

-- | Inverse of 'sigmoidTanh'
invSigmoidTanh :: Floating a => a -> a
invSigmoidTanh x = atanh (2 * x - 1)

gradualTransition :: Floating a =>
                     a                  -- ^ tolerance (e.g. 0.01 for 1%)
                  -> a                  -- ^ duration of the transition phase
                  -> a                  -- ^ old value
                  -> a                  -- ^ new value
                  -> a                  -- ^ time since start of transition
                  -> a
gradualTransition tolerance duration old new t =
  old * (new / old) ** sigmoidTanh (t / duration * (t1 + t0) - t0)
  where t0 = -invSigmoidTanh (log (1 + tolerance) / log (new / old))
        t1 = invSigmoidTanh (log (1 - tolerance) / log (new / old) + 1)

_s :: String -> String
_s = id

(=.) :: String -> a -> (String, a)
(=.) = (,)

traceS :: Show a => a -> a
traceS x = traceShow x x

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

-- | Perform the given action with a different working directory.
withDir :: FilePath -> IO a -> IO a
withDir dir action = do
  dir' <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir) (setCurrentDirectory dir') action

ensureDirectoryExist :: MonadIO m => FilePath -> m FilePath
ensureDirectoryExist dir = do
  liftIO (createDirectoryIfMissing True dir)
  pure dir
