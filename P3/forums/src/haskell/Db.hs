
{-# LANGUAGE OverloadedStrings  #-}

module Db
    ( Connection, openDb, closeDb
    , runDbTran, DbM
    , Key(..), DbEntity(..)
    , get, getJust, select, selectOrder, add, set, update, delete
    , FromField(..), ToField(..), FromRow(..), ToRow(..), field
    )
where
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import Data.Text as T
import qualified Data.List as L
import Data.Int
import Data.Semigroup
import System.Directory  -- doesFileExist
import System.IO.Error  -- mkIOError, ...
import Control.Monad.IO.Class   -- imports liftIO

-- ---------------------------------------------------------------
-- Key / DbEntity

newtype Key a = Key { getKey :: Int64 }
    deriving (Eq, Show, Read)

class (FromRow a, ToRow a) => DbEntity a where
    tableName :: proxy a -> Text
    keyName :: proxy a -> Text
    columnNames :: proxy a -> [Text]
    -- Default definitions:
    keyName _ = "id"

instance FromField (Key a) where
    fromField f = Key <$> fromField f

instance ToField (Key a) where
    toField (Key x) = toField x

-- ---------------------------------------------------------------
-- Data base connection

openDb :: Text -> IO Connection
openDb patht = do
    let path = unpack patht
    ok <- doesFileExist path
    if ok then do
        conn <- open path
        execute_ conn "PRAGMA foreign_keys=ON;"
        pure conn
    else
        ioError $ mkIOError doesNotExistErrorType "Cannot open data base file" Nothing (Just path)

closeDb :: Connection -> IO ()
closeDb conn =
    close conn

-- ---------------------------------------------------------------
-- Data base access

newtype DbM a = DbM { runDbM :: Connection -> IO a }

instance Functor DbM where
    fmap f (DbM h) = DbM $ \ conn -> fmap f (h conn)

instance Applicative DbM where
    pure x = DbM $ \ conn -> pure x
    DbM hf <*> DbM hx = DbM $ \ conn -> do
        f <- hf conn
        x <- hx conn
        pure (f x)

instance Monad DbM where
    DbM hx >>= f = DbM $ \ conn -> do
        x <- hx conn
        let DbM hy = f x
        hy conn

instance MonadIO DbM where
    liftIO io = DbM $ \ conn -> io


runDbTran :: MonadIO m => Connection -> DbM a -> m a
runDbTran conn dbm = do
    liftIO $ runDbM dbm conn


tshow :: Text -> Text
tshow name = pack $ show name

interCommas :: [Text] -> Text
interCommas names = T.intercalate "," names

get :: DbEntity a => Key a -> DbM (Maybe a)
get k = DbM $ \ conn -> do
        let tabName = tshow $ tableName k
            kName = tshow $ keyName k
            colNames = tshow <$> columnNames k
            q = Query $ "SELECT " <> interCommas colNames <> " FROM " <> tabName <> " WHERE " <> kName <> " = ?"
        rows <- query conn q (Only k)
        case rows of
            [] -> pure Nothing
            [row] -> pure $ Just row

getJust :: DbEntity a => Key a -> DbM a
getJust k =
    get k >>= maybe (fail $ "Invalid foreign key " <> show k) pure

select :: DbEntity a => (Key a -> a -> Bool) -> DbM [(Key a, a)]
select fwhere = DbM $ \ conn -> do
    let proxyForFun :: (Key a -> a -> Bool) -> Maybe a
        proxyForFun f = Nothing
        proxy = proxyForFun fwhere
        tabName = tshow $ tableName proxy
        kName = tshow $ keyName proxy
        colNames = tshow <$> columnNames proxy
        q = Query $ "SELECT " <> interCommas (kName : colNames) <> " FROM " <> tabName
        unrow (Only k :. entity) = (k, entity)
    rows <- query_ conn q
    pure $ L.filter (uncurry fwhere) $ unrow <$> rows

selectOrder :: (DbEntity a, Ord fld) => (Key a -> a -> Bool) -> (a -> fld) -> DbM [(Key a, a)]
selectOrder fwhere forderby =
    L.sortOn (forderby . snd) <$> select fwhere

add :: DbEntity a => a -> DbM (Key a)
add value = DbM $ \ conn -> do
    let tabName = tshow $ tableName $ Just value
        colNames = tshow <$> (columnNames $ Just value)
        q = Query $ "INSERT INTO " <> tabName <> " (" <> interCommas colNames <> ") VALUES (" <> interCommas (const "?" <$> colNames) <> ")"
    execute conn q value
    Key <$> lastInsertRowId conn

set :: DbEntity a => Key a -> a -> DbM ()
set k value = DbM $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "UPDATE " <> tabName <> " SET " <> interCommas ((<> "=?") <$> colNames) <> " WHERE " <> kName <> " = ?"
    execute conn q (value :. Only k)

update :: DbEntity a => Key a -> (a -> a) -> DbM ()
update k f = do
    mbvalue <- get k
    case mbvalue of
        Just value -> set k (f value)
        Nothing -> pure ()

delete :: DbEntity a => Key a -> DbM ()
delete k = DbM $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "DELETE FROM " <> tabName <> " WHERE " <> kName <> " = ?"
    execute conn q (Only k)

