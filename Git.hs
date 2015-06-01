module Git where
import Prelude ()
import Common
import System.Directory
import System.FilePath
import System.Process

newtype Git = Git FilePath

-- | Initialize the path as a git repository.
--   Does nothing if already initialized.
gitInit :: MonadIO m => FilePath -> m Git
gitInit dir = liftIO $ do
  createDirectoryIfMissing True dir
  withDir dir $ do
    callProcess "git" ["init", "-q"]
    callProcess "git" ["config", "user.name", "Bot"]
    callProcess "git" ["config", "user.email", "<bot@nowhere>"]
  return (Git dir)

-- | Add a file with the give relative path and contents and stage it.
gitAddFile :: MonadIO m => Git -> FilePath -> ByteString -> m ()
gitAddFile (Git dir) name contents = liftIO . withDir dir $ do
  createDirectoryIfMissing True (takeDirectory name)
  writeFileB tmpName contents
  renameFile tmpName name
  callProcess "git" ["add", name]
  where tmpName = name <> ".tmp#"

-- | Commit all the staged changes.  Does nothing if there are no changes.
gitCommit :: MonadIO m => Git -> String -> m ()
gitCommit (Git dir) message = liftIO . withDir dir $ do
  shortstat <- readProcess "git" ["diff", "--cached", "--shortstat"] ""
  -- don't commit if there's nothing to do
  when (shortstat /= "") $ do
    callProcess "git" ["commit", "-m", message, "-q"]
