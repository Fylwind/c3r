{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings,
             TypeSynonymInstances #-}
module Database
       ( module Database
       , SQLiteHandle
       ) where
import Prelude ()
import Common
import Data.Int (Int64)
import Database.SQLite (SQLiteHandle)
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

class SQLiteValue a where
  fromSQLiteValue :: SQL.Value -> Maybe a
  toSQLiteValue   :: a -> SQL.Value

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

instance SQLiteValue UTCTime where
  fromSQLiteValue (SQL.Text s) = case readsPrec 0 s of
    [(i, "")] -> Just i
    _         -> Nothing
  fromSQLiteValue _            = Nothing
  toSQLiteValue = SQL.Text . show

-- Note that JSON.Null is not stored as SQL.Null, but as SQL.Text "null"
-- This is crucial: we use SQL.Null to indicate a deleted entry in user_updates
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

(=..) :: SQLiteValue a => String -> a -> (String, SQL.Value)
x =.. y = x =. toSQLiteValue y

withConnection :: String -> (SQLiteHandle -> IO c) -> IO c
withConnection name = bracket (SQL.openConnection name) SQL.closeConnection

withTransaction :: (MonadIO m, MonadMask m) =>
                   SQLiteHandle -> m a -> m a
withTransaction db =
  bracket_
  (sqlExec_' db "BEGIN TRANSACTION;")
  (sqlExec_' db "COMMIT TRANSACTION;")

sqlExec' :: (MonadIO m, SQL.SQLiteResult a) =>
            SQLiteHandle -> String -> m [[SQL.Row a]]
sqlExec' db stmt =
  liftIO $ throwIfLeft DatabaseError =<< SQL.execStatement db stmt

sqlExec_' :: MonadIO m => SQLiteHandle -> String -> m ()
sqlExec_' db stmt =
  liftIO $ throwIfJust DatabaseError =<< SQL.execStatement_ db stmt

sqlExec :: (MonadIO m, SQLiteRecord r, SQL.SQLiteResult a) =>
           SQLiteHandle -> r -> String -> m [[SQL.Row a]]
sqlExec db params stmt =
  liftIO $ throwIfLeft DatabaseError =<<
           SQL.execParamStatement db stmt
             (mapFst (":" <>) <$> toSQLiteRecord params)

sqlExec_ :: (MonadIO m, SQLiteRecord r) =>
            SQLiteHandle -> r -> String -> m ()
sqlExec_ db params stmt =
  liftIO $ throwIfJust DatabaseError =<<
           SQL.execParamStatement_ db stmt
             (mapFst (":" <>) <$> toSQLiteRecord params)

insertRow :: (MonadIO m, SQLiteRecord r) =>
             SQLiteHandle -> String -> r -> m ()
insertRow db tableName record =
  sqlExec_ db record' $
    "INSERT INTO " <> tableName <> " (" <> List.intercalate ", " fields <>
    ") VALUES (" <> List.intercalate ", " ((":" <>) <$> fields) <> ");"
  where fields  = fst <$> record'
        record' = toSQLiteRecord record

createTable :: MonadIO m => SQLiteHandle -> String -> [String] -> m ()
createTable db name fields =
  sqlExec_' db $
    "CREATE TABLE IF NOT EXISTS " <> name <>
    " (" <> intercalate ", " fields <> ");"
