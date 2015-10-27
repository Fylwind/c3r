{-# LANGUAGE DeriveDataTypeable, DefaultSignatures, FlexibleInstances,
             OverloadedStrings, TypeSynonymInstances #-}
module Database where
import Prelude ()
import Common
import Data.Int (Int64)
import Data.Time (UTCTime)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Lazy as BytesL
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as TextL
import qualified Data.Text.Encoding as Text
import qualified Database.SQLite as SQL

newtype DatabaseError = DatabaseError String deriving (Show, Typeable)
instance Exception DatabaseError

data Database =
  Database
  { db_handle :: SQL.SQLiteHandle
  , db_lock   :: Maybe (MVar ())
  }

class SQLiteValue a where
  fromSQLiteValue :: SQL.Value -> Maybe a
  toSQLiteValue   :: a -> SQL.Value

  default fromSQLiteValue :: Read a => SQL.Value -> Maybe a
  fromSQLiteValue (SQL.Text s) = maybeRead s
  fromSQLiteValue _            = Nothing

  default toSQLiteValue :: Show a => a -> SQL.Value
  toSQLiteValue = SQL.Text . show

instance SQLiteValue SQL.Value where
  fromSQLiteValue = Just
  toSQLiteValue = id

-- caveat: the wrapped type must not use NULL to represent its data!
instance SQLiteValue a => SQLiteValue (Maybe a) where
  fromSQLiteValue SQL.Null = Nothing
  fromSQLiteValue x        = Just (fromSQLiteValue x)
  toSQLiteValue Nothing  = SQL.Null
  toSQLiteValue (Just x) = case toSQLiteValue x of
    SQL.Null -> error "toSQLiteValue[Maybe]: did not expect Null"
    x'       -> x'

instance SQLiteValue ByteString where
  fromSQLiteValue (SQL.Blob s) = Just s
  fromSQLiteValue _            = Nothing
  toSQLiteValue = SQL.Blob

instance SQLiteValue ByteStringL where
  fromSQLiteValue (SQL.Blob s) = Just (BytesL.fromStrict s)
  fromSQLiteValue _            = Nothing
  toSQLiteValue = SQL.Blob . BytesL.toStrict

instance SQLiteValue Int where
  fromSQLiteValue = integralFromSQLiteValue
  toSQLiteValue = integralToSQLiteValue

instance SQLiteValue Int64 where
  fromSQLiteValue = integralFromSQLiteValue
  toSQLiteValue = SQL.Int

instance SQLiteValue Integer where
  fromSQLiteValue (SQL.Int  i) = Just (toInteger i)
  fromSQLiteValue (SQL.Text s) = case readsPrec 0 s of
    [(i, "")] -> Just i
    _         -> Nothing
  fromSQLiteValue _            = Nothing
  toSQLiteValue i = case boundedFromIntegral i of
    Just i' -> SQL.Int i'
    Nothing -> SQL.Text (show i)

instance SQLiteValue String where
  fromSQLiteValue (SQL.Text s) = Just s
  fromSQLiteValue _            = Nothing
  toSQLiteValue = SQL.Text

instance SQLiteValue Text where
  fromSQLiteValue (SQL.Text s) = Just (Text.pack s)
  fromSQLiteValue _            = Nothing
  toSQLiteValue = SQL.Text . Text.unpack

instance SQLiteValue UTCTime

instance SQLiteValue JSON.Value where
  fromSQLiteValue (SQL.Text s) = JSON.decode . BytesL.fromStrict $
                                 Text.encodeUtf8 (Text.pack s)
  fromSQLiteValue _            = Nothing
  toSQLiteValue = SQL.Text . TextL.unpack .
                  TextL.toLazyText . JSON.encodeToTextBuilder

class SQLiteRecord r where
  fromSQLiteRecord :: SQL.Row SQL.Value -> Maybe r
  toSQLiteRecord :: r -> SQL.Row SQL.Value

instance SQLiteRecord (SQL.Row SQL.Value) where
  fromSQLiteRecord = Just
  toSQLiteRecord = id

integralFromSQLiteValue :: (Bounded b, Integral b) => SQL.Value -> Maybe b
integralFromSQLiteValue v = do
  i <- fromSQLiteValue v
  boundedFromIntegral (i :: Integer)

integralToSQLiteValue :: Int -> SQL.Value
integralToSQLiteValue = toSQLiteValue . toInteger

boundedFromIntegral :: (Integral a, Integral b, Bounded b) => a -> Maybe b
boundedFromIntegral i
  | toInteger i >= toInteger (minBound `asTypeOf` i') &&
    toInteger i <= toInteger (maxBound `asTypeOf` i') = Just i'
  | otherwise                                         = Nothing
  where i' = fromIntegral i

withConnection :: String -> (Database -> IO c) -> IO c
withConnection name = bracket initialize finalize
  where initialize = Database <$> SQL.openConnection name
                              <*> (Just <$> newMVar ())
        finalize   = SQL.closeConnection . db_handle

-- | [Internal] Execute an 'IO' action while the locking the database.
withLock :: MonadIO m => Database -> (SQL.SQLiteHandle -> IO a) -> m a
withLock (Database conn Nothing)     = liftIO .                        ($ conn)
withLock (Database conn (Just lock)) = liftIO . withMVar lock . pure . ($ conn)

-- | [Internal] Hide the lock from the 'Database' object so that any function
--   that uses this object will not lock.  (This is allows nesting of
--   operations.)
hideLock :: Database -> Database
hideLock db = db{db_lock = Nothing}

sqlExec' :: (MonadIO m, SQL.SQLiteResult a) =>
            Database -> String -> m [[SQL.Row a]]
sqlExec' db stmt = withLock db $ \ conn ->
  throwIfLeft DatabaseError =<< SQL.execStatement conn stmt

sqlExec_' :: MonadIO m => Database -> String -> m ()
sqlExec_' db stmt = withLock db $ \ conn ->
  throwIfJust DatabaseError =<< SQL.execStatement_ conn stmt

sqlExec :: (MonadIO m, SQLiteRecord r, SQL.SQLiteResult a) =>
           Database -> r -> String -> m [[SQL.Row a]]
sqlExec db params stmt = withLock db $ \ conn ->
  throwIfLeft DatabaseError =<<
  SQL.execParamStatement conn stmt
    (first (":" <>) <$> toSQLiteRecord params)

sqlExec_ :: (MonadIO m, SQLiteRecord r) =>
            Database -> r -> String -> m ()
sqlExec_ db params stmt = withLock db $ \ conn ->
  throwIfJust DatabaseError =<<
  SQL.execParamStatement_ conn stmt
    (first (":" <>) <$> toSQLiteRecord params)

insertRow :: (MonadIO m, SQLiteRecord r) =>
             Database -> String -> r -> m Integer
insertRow db tableName record = withLock db $ \ _ -> do
  sqlExec_ db' record' $
    "INSERT INTO " <> tableName <> " (" <> List.intercalate ", " fields <>
    ") VALUES (" <> List.intercalate ", " ((":" <>) <$> fields) <> ");"
  SQL.getLastRowID (db_handle db')
  where fields  = fst <$> record'
        record' = toSQLiteRecord record
        db'     = hideLock db

createTable :: MonadIO m => Database -> String -> [String] -> m ()
createTable db name fields =
  sqlExec_' db ("CREATE TABLE IF NOT EXISTS " <> name <>
                " (" <> intercalate ", " fields <> ");")
