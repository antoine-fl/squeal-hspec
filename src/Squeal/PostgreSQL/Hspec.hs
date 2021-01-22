{-|
Helpers for creating database tests with hspec and squeal, inspired by Jonathan Fischoff's
[hspec-pg-transact](http://hackage.haskell.org/package/hspec-pg-transact).

This uses @tmp-postgres@ to automatically and connect to a temporary instance of postgres on a random port.

Tests can be written with 'itDB' which is wrapper around 'it' that uses the passed in 'TestDB' to run a db transaction automatically for the test.

The libary also provides a few other functions for more fine grained control over running transactions in tests.
-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeInType       #-}
{-# LANGUAGE TypeOperators    #-}
module Squeal.PostgreSQL.Hspec
where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Base     (liftBase)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BSC
import           Database.Postgres.Temp as Temp
import           Squeal.PostgreSQL
import           System.Environment     (lookupEnv)
import           Test.Hspec

data TestDB a = TestDB
  { tempDB           :: Maybe Temp.DB
  -- ^ Handle for temporary @postgres@ process
  , pool             :: Pool (K Connection a)
  -- ^ Pool of 50 connections to the temporary @postgres@
  , connectionString :: ByteString
  }

type Fixtures db a = (Pool (K Connection db) -> IO a)
type Actions db a = PQ db db IO a
type FixtureContext db fix = (TestDB db, fix)
type Migrations def from to = Path (Migration def) from to

testDBEnv :: String
testDBEnv = "TEST_DB_CONNECTION_STRING"

getOrCreateConnectionString :: IO (ByteString, Maybe Temp.DB)
getOrCreateConnectionString = do
  hasConnectionString <- lookupEnv testDBEnv
  maybe createTempDB (pure . (, Nothing) . BSC.pack) hasConnectionString

createTempDB :: IO (ByteString, Maybe Temp.DB)
createTempDB = do
  tempDB <- either throwIO return =<< Temp.start
  let connectionString = Temp.toConnectionString tempDB
  pure (connectionString, Just tempDB)

-- | Start a temporary @postgres@ process and create a pool of connections to it
setupDB
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Migrations def schema0 schema
  -> Fixtures schema fix
  -> IO (FixtureContext schema fix)
setupDB migration fixtures = do
  (connectionString, tempDB) <- getOrCreateConnectionString
  BSC.putStrLn connectionString
  let singleStripe = 1
      keepConnectionForOneHour = 3600
      poolSizeOfFifty = 50
  pool <- createConnectionPool
     connectionString
     singleStripe
     keepConnectionForOneHour
     poolSizeOfFifty
  withConnection connectionString (migrateUp migration)
  res <- fixtures pool
  pure (TestDB {..}, res)

-- | Drop all the connections and shutdown the @postgres@ process
teardownDB
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Migrations def schema0 schema
  -> TestDB a
  -> IO ()
teardownDB migration TestDB {..} = do
  withConnection connectionString (migrateDown migration)
  destroyConnectionPool pool
  maybe (pure ()) (void . Temp.stop) tempDB

-- | Run an 'IO' action with a connection from the pool
withPool :: TestDB db -> Actions db a -> IO a
withPool testDB = liftBase . usingConnectionPool (pool testDB)

-- | Run an 'DB' transaction, using 'transactionally_'
withDB :: Actions db a -> TestDB db -> IO a
withDB action testDB =
  usingConnectionPool (pool testDB) (transactionally_ action)

-- | Flipped version of 'withDB'
runDB :: TestDB db -> Actions db a -> IO a
runDB = flip withDB

withFixture :: (fix -> Actions db a) -> FixtureContext db fix -> IO a
withFixture action (db, fix) =
  usingConnectionPool (pool db) (transactionally_ $ action fix)

withoutFixture :: Actions db a -> FixtureContext db fix -> IO a
withoutFixture action (db, _) =
  usingConnectionPool (pool db) (transactionally_ action)

-- | Helper for writing tests. Wrapper around 'it' that uses the passed
--   in 'TestDB' to run a db transaction automatically for the test.
itDB :: String -> Actions db a -> SpecWith (FixtureContext db ())
itDB msg action = it msg $ void . withoutFixture action

-- | Helper for writing tests. Wrapper around 'it' that uses the passed
-- in 'TestDB' to run a db transaction automatically for the test,
-- plus the result of the fixtures.
itDBF :: String -> (fix -> Actions db a) -> SpecWith (FixtureContext db fix)
itDBF msg action = it msg $ void . withFixture action

itDBF_ :: String -> Actions db a -> SpecWith (FixtureContext db fix)
itDBF_ msg action = it msg $ void . withoutFixture action

-- | Wraps 'describe' with a
--
-- @
--   'beforeAll' ('setupDB' migrate)
-- @
--
-- hook for creating a db and a
--
-- @
--   'afterAll' 'teardownDB'
-- @
--
-- hook for stopping a db.
describeDB
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Migrations def db0 db
  -> Fixtures db ()
  -> String
  -> SpecWith (FixtureContext db ())
  -> Spec
describeDB migrate fixture str =
  beforeAll (setupDB migrate fixture) . afterAll (teardownDB migrate . fst) . describe str

-- | Like `decribeDB`, but allow fixtures to pass
-- | a result to all specs
describeFixtures
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Migrations def db0 db
  -> Fixtures db fix
  -> String
  -> SpecWith (FixtureContext db fix)
  -> Spec
describeFixtures migrate fixture str =
  beforeAll (setupDB migrate fixture) . afterAll (teardownDB migrate . fst) . describe str
