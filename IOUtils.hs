{-# LANGUAGE FlexibleContexts #-}
module IOUtils (module IOUtils, Handle, stderr, stdout) where
import Control.Exception.Lifted (catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import System.IO (Handle, stderr, stdout)
import qualified Data.Text.IO as Text
import qualified System.IO as IO

catchIOError :: MonadBaseControl IO m => m a -> (IOError -> m a) -> m a
catchIOError = catch

hPutStrLn :: MonadIO m => Handle -> String -> m ()
hPutStrLn h = liftIO . IO.hPutStrLn h

hPutStrLn' :: MonadIO m => Handle -> String -> m ()
hPutStrLn' h = (>> hFlush h) . hPutStrLn h

hPrint :: (MonadIO m, Show a) => Handle -> a -> m ()
hPrint h = liftIO . IO.hPrint h

hPrint' :: (MonadIO m, Show a) => Handle -> a -> m ()
hPrint' h = (>> hFlush h) . hPrint h

putText :: MonadIO m => Text -> m ()
putText = liftIO . Text.putStr

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . Text.putStrLn

putText' :: MonadIO m => Text -> m ()
putText' = (>> hFlush stdout) . putText

putTextLn' :: MonadIO m => Text -> m ()
putTextLn' = (>> hFlush stdout) . putTextLn

hPutText :: MonadIO m => Handle -> Text -> m ()
hPutText h = liftIO . Text.hPutStr h

hPutTextLn :: MonadIO m => Handle -> Text -> m ()
hPutTextLn h = liftIO . Text.hPutStrLn h

hPutText' :: MonadIO m => Handle -> Text -> m ()
hPutText' h = (>> hFlush stdout) . liftIO . Text.hPutStr h

hPutTextLn' :: MonadIO m => Handle -> Text -> m ()
hPutTextLn' h = (>> hFlush stdout) . liftIO . Text.hPutStrLn h

hFlush :: MonadIO m => Handle -> m ()
hFlush = liftIO . hFlush
