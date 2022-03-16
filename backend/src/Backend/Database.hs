{-# LANGUAGE DeriveGeneric, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Backend.Database
  ( AppDb(..)
  , QueryError(..)
  , appDb
  , maybeRow
  , openAppDb
  , rowList
  , singleRow
  ) where

import           Control.Exception          (Exception)
import           Control.Monad.Error.Class  (MonadError, throwError)
import           Data.ByteString            (ByteString)
import           Data.Conduit               (ConduitT, (.|))
import qualified Data.Conduit               as Conduit
import qualified Data.Conduit.List          as Conduit
import           Data.Int                   (Int32)
import           Database.Beam              (Database, DatabaseSettings, MonadIO, TableEntity,
                                             defaultDbSettings, liftIO)
import           Database.Beam.Postgres     (Postgres)
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           GHC.Generics               (Generic)

import           Backend.Database.Teams.Team               (TeamT)
import           Backend.Database.Players.Player           (PlayerT)
import           Backend.Database.Matchups.Matchup         (MatchupT)
import           Backend.Database.Performances.Performance (PerformanceT)

data AppDb f = AppDb
  { appTeams        :: f (TableEntity TeamT)
  , appPlayers      :: f (TableEntity PlayerT)
  , appMatchups     :: f (TableEntity MatchupT)
  , appPerformances :: f (TableEntity PerformanceT)
  } deriving (Generic)

instance Database Postgres AppDb

newtype QueryError = UnexpectedAmountOfRows Int32
  deriving Show

instance Exception QueryError

appDb :: DatabaseSettings Postgres AppDb
appDb = defaultDbSettings

openAppDb :: MonadIO m => ByteString -> m Connection
openAppDb = liftIO . connectPostgreSQL

maybeRow :: Monad m => ConduitT () a m () -> m (Maybe a)
maybeRow c = Conduit.runConduit (c .| Conduit.await)

singleRow :: (MonadError QueryError m) => ConduitT () a m () -> m a
singleRow c = maybe (throwError (UnexpectedAmountOfRows 0)) pure =<< maybeRow c

rowList :: Monad m => ConduitT () a m () -> m [a]
rowList c = Conduit.runConduit (c .| Conduit.consume)
