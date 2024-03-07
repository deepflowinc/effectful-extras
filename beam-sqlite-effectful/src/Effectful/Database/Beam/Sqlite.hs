{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.Database.Beam.Sqlite (
  Sqlite,
  SqliteTransaction,
  liftSqliteM,
  runSqlite,
  runSqliteDebug,
  SqliteDb (..),
  notransact,
  transact,
  transactImmediate,
  transactExclusive,
  insertMany,
  insert_,
  update_,
  module Query,
  selectMany,
  selectOne,
  selectFirst,
  withSqlPool,
  SqlitePool (),
) where

import Control.Concurrent (getNumCapabilities)
import Control.Exception.Safe
import Control.Monad (when)
import Data.Function (fix, (&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Pool
import Data.Text qualified as T
import Database.Beam (DatabaseEntity, QExpr, SqlInsertValues)
import Database.Beam as Query (insertValues)
import Database.Beam qualified as Beam
import Database.Beam.Backend.SQL.BeamExtensions qualified as Beam
import Database.Beam.Sqlite qualified as Sqlite
import Database.SQLite.Simple (Connection, Error (..), SQLError (..))
import Database.SQLite.Simple qualified as Sqlite
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Static
import Effectful.Log
import Effectful.Random.Static (Random, uniformR)
import GHC.Generics (Generic)
import Path
import Text.Printf (printf)

data Sqlite :: Effect

newtype SqlitePool = SqlitePool {getPool :: Pool Connection}

type instance DispatchOf Sqlite = 'Static 'WithSideEffects

data instance StaticRep Sqlite = Sqlite (String -> IO ()) SqlitePool

data SqliteTransaction :: Effect

type instance DispatchOf SqliteTransaction = 'Static 'NoSideEffects

data instance StaticRep SqliteTransaction = SqliteT (String -> IO ()) Connection

data SqliteDb
  = DbFile (Path Abs File)
  | DbPool SqlitePool
  deriving (Generic)

liftSqliteM :: (SqliteTransaction :> es) => Sqlite.SqliteM a -> Eff es a
liftSqliteM act = do
  SqliteT logger conn <- getStaticRep
  unsafeEff_ $ Sqlite.runBeamSqliteDebug logger conn act

selectMany ::
  (SqliteTransaction :> es, Beam.FromBackendRow Sqlite.Sqlite a) =>
  Beam.SqlSelect Sqlite.Sqlite a ->
  Eff es [a]
selectMany = liftSqliteM . Beam.runSelectReturningList

selectOne ::
  (SqliteTransaction :> es, Beam.FromBackendRow Sqlite.Sqlite a) =>
  Beam.SqlSelect Sqlite.Sqlite a ->
  Eff es (Maybe a)
selectOne = liftSqliteM . Beam.runSelectReturningOne

selectFirst ::
  (SqliteTransaction :> es, Beam.FromBackendRow Sqlite.Sqlite a) =>
  Beam.SqlSelect Sqlite.Sqlite a ->
  Eff es (Maybe a)
#if MIN_VERSION_beam_core(0,10,0)
selectFirst = liftSqliteM . Beam.runSelectReturningFirst
#else
selectFirst q = selectMany q <&> \case
  [] -> Nothing
  x : _ -> Just x
#endif

insertMany ::
  ( SqliteTransaction :> es
  , Beam.Beamable table
  , Beam.FromBackendRow Sqlite.Sqlite (table Identity)
  ) =>
  DatabaseEntity Sqlite.Sqlite db (Beam.TableEntity table) ->
  SqlInsertValues
    Sqlite.Sqlite
    (table (QExpr Sqlite.Sqlite s)) ->
  Eff es [table Identity]
insertMany db = liftSqliteM . Beam.runInsertReturningList . Beam.insert db

insert_ ::
  ( SqliteTransaction :> es
  , Beam.Beamable table
  ) =>
  DatabaseEntity Sqlite.Sqlite db (Beam.TableEntity table) ->
  SqlInsertValues
    Sqlite.Sqlite
    (table (QExpr Sqlite.Sqlite s)) ->
  Eff es ()
insert_ db = liftSqliteM . Beam.runInsert . Beam.insert db

update_ :: (SqliteTransaction :> es) => Beam.SqlUpdate Sqlite.Sqlite tbl -> Eff es ()
{-# INLINE update_ #-}
update_ = liftSqliteM . Beam.runUpdate

withResourceEff :: Pool a -> (a -> Eff es r) -> Eff es r
withResourceEff pool act = mask $ \unmask -> do
  (res, localPool) <- unsafeEff_ $ takeResource pool
  r <- unmask (act res) `onException` unsafeEff_ (destroyResource pool localPool res)
  unsafeEff_ $ putResource localPool res
  pure r

retryBusy :: (Random :> es, Concurrent :> es, Log :> es) => Eff es r -> Eff es r
retryBusy act =
  (10 :: Int, 0.5e6 :: Double) & fix \self (!left, !wait) -> do
    when (left <= 0) $
      throwM $
        SQLError
          { sqlErrorDetails = "Busy retrial limit exceeded"
          , sqlErrorContext = "retryBusy"
          , sqlError = ErrorBusy
          }
    act `catch` \exc@SQLError {..} ->
      case sqlError of
        ErrorBusy -> do
          wait' <- uniformR (1e-6, wait)
          logAttention_ $
            "ErrorBusy detected. Retrying after " <> T.pack (printf "%.06f" (wait' * 1e-6)) <> " seconds..."
          threadDelay $ floor wait'
          self (left - 1, wait * 1.5)
        _ -> throwM exc

notransact ::
  (Sqlite :> es, Concurrent :> es, Log :> es, Random :> es) =>
  Eff (SqliteTransaction ': es) a ->
  Eff es a
notransact act = do
  Sqlite logg (SqlitePool mconn) <- getStaticRep
  withResourceEff mconn $ \conn ->
    evalStaticRep (SqliteT logg conn) $ retryBusy act

transact ::
  (Sqlite :> es, Concurrent :> es, Log :> es, Random :> es) =>
  Eff (SqliteTransaction ': es) a ->
  Eff es a
transact act = do
  Sqlite logg (SqlitePool mconn) <- getStaticRep
  withResourceEff mconn $ \conn ->
    unsafeLiftMapIO (Sqlite.withTransaction conn) $
      evalStaticRep (SqliteT logg conn) $
        retryBusy act

transactImmediate ::
  (Sqlite :> es, Concurrent :> es, Log :> es, Random :> es) =>
  Eff (SqliteTransaction ': es) a ->
  Eff es a
transactImmediate act = do
  Sqlite logg (SqlitePool mconn) <- getStaticRep
  withResourceEff mconn $ \conn ->
    unsafeLiftMapIO (Sqlite.withImmediateTransaction conn) $
      evalStaticRep (SqliteT logg conn) $
        retryBusy act

transactExclusive ::
  (Sqlite :> es, Concurrent :> es, Log :> es, Random :> es) =>
  Eff (SqliteTransaction ': es) a ->
  Eff es a
transactExclusive act = do
  Sqlite logg (SqlitePool mconn) <- getStaticRep
  withResourceEff mconn $ \conn ->
    unsafeLiftMapIO (Sqlite.withExclusiveTransaction conn) $
      evalStaticRep (SqliteT logg conn) $
        retryBusy act

withSqlPool :: (IOE :> es) => Path Abs File -> (SqlitePool -> Eff es a) -> Eff es a
withSqlPool db =
  bracket
    ( liftIO $ do
        num <- getNumCapabilities
        fmap SqlitePool $
          newPool $
            defaultPoolConfig
              ( do
                  conn <- Sqlite.open (toFilePath db)
                  Sqlite.execute_ conn "PRAGMA busy_timeout=3000;"
                  pure conn
              )
              Sqlite.close
              0.5
              num
    )
    (liftIO . destroyAllResources . getPool)

runSqlite :: (IOE :> es) => SqliteDb -> Eff (Sqlite ': es) a -> Eff es a
{-# INLINE runSqlite #-}
runSqlite (DbFile dbPath) act =
  withSqlPool dbPath $ \pool ->
    evalStaticRep (Sqlite (const $ pure ()) pool) act
runSqlite (DbPool pool) act =
  evalStaticRep (Sqlite (const $ pure ()) pool) act

runSqliteDebug ::
  (IOE :> es, Log :> es) =>
  SqliteDb ->
  Eff (Sqlite ': es) a ->
  Eff es a
{-# INLINE runSqliteDebug #-}
runSqliteDebug (DbFile sql) act = withSqlPool sql $ \pool -> do
  withRunInIO $ \runInIO ->
    runInIO $
      evalStaticRep (Sqlite (runInIO . localDomain "sql" . logTrace_ . T.pack) pool) act
runSqliteDebug (DbPool pool) act = do
  withRunInIO $ \runInIO ->
    runInIO $
      evalStaticRep (Sqlite (runInIO . localDomain "sql" . logTrace_ . T.pack) pool) act
