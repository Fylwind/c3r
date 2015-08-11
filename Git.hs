module Git where
import Prelude ()
import Common
import System.Directory
import System.FilePath
import System.Process

data Git = Git { git_dir :: FilePath, git_lock :: MVar () }

withGit :: MonadIO m => Git -> IO a -> m a
withGit (Git dir lock) = liftIO . withMVar lock . pure . withDir dir

-- | Initialize the path as a git repository.
--   Does nothing if already initialized.
gitInit :: MonadIO m => FilePath -> m Git
gitInit dir = liftIO $ do
  createDirectoryIfMissing True dir
  withDir dir $ do
    callProcess "git" ["init", "-q"]
    callProcess "git" ["config", "user.name", "Bot"]
    callProcess "git" ["config", "user.email", "<bot@nowhere>"]
  lock <- newMVar ()
  return (Git dir lock)

-- | Add a file with the give relative path and contents and stage it.
gitAddFile :: MonadIO m => Git -> FilePath -> ByteString -> m ()
gitAddFile git name contents = withGit git $ do
  createDirectoryIfMissing True (takeDirectory name)
  writeFileB tmpName contents
  renameFile tmpName name
  callProcess "git" ["add", name]
  where tmpName = name <> ".tmp#"

-- | Commit all the staged changes.  Does nothing if there are no changes.
gitCommit :: MonadIO m => Git -> String -> m ()
gitCommit git message = withGit git $ do
  shortstat <- readProcess "git" ["diff", "--cached", "--shortstat"] ""
  -- don't commit if there's nothing to do
  when (shortstat /= "") $ do
    callProcess "git" ["commit", "-m", message, "-q"] `onException`
      print("shortstat=",shortstat)

-- | Garbage collection.
gitGC :: MonadIO m => Git -> m ()
gitGC git = withGit git $ do
  callProcess "git" ["gc", "--quiet"]

-- | Garbage collection.
gitGCAgg :: MonadIO m => Git -> m ()
gitGCAgg git = withGit git $ do
  callProcess "git" ["gc", "--aggressive", "--quiet"]
