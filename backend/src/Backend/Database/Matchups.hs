{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Backend.Database.Matchups
  ( create
  , matchupExists
  , find
  ) where

import           Common.Api.Matchups.Matchup
import           Control.Lens
import           Control.Monad.Error.Class       (MonadError)
import           Control.Monad.Fail              (MonadFail)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader.Class      (MonadReader, ask)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Data.Int                        (Int32)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import           Database.Beam.Postgres.Extended (Nullable, PgInsertReturning, PgQExpr, Postgres, Postgres,
                                                  PgUpdateReturning, Q, aggregate_, all_, array_,
                                                  conflictingFields, count_, default_, delete, desc_, exists_,
                                                  group_, guard_, in_, insert, insertExpressions,
                                                  insertReturning, insertValues, isSubsetOf_, just_,
                                                  leftJoin_, limit_, offset_, onConflict, onConflictDefault,
                                                  onConflictDoNothing, orderBy_, pgArrayAgg, pgBoolOr,
                                                  primaryKey, references_, runDelete, runInsert,
                                                  runInsertReturning, runSelect, runUpdateReturning, select,
                                                  updateReturning, val_, (&&.), (<-.), (==.),
                                                  HasSqlEqualityCheck)
import           Database.PostgreSQL.Simple      (Connection)

import           Backend.Database              (AppDb(appMatchups), QueryError,
                                                  appDb, maybeRow, singleRow)
import           Backend.Database.Matchups.Matchup

insertMatchup
  :: Matchup -> PgInsertReturning MatchupRow
insertMatchup m
  = insertReturning
    (appMatchups appDb)
    (insertExpressions
      [ MatchupT
          { _matchup_t_id = val_ $ m ^. matchup_id
          , _matchup_t_teamName = val_ $ m ^. matchup_teamName
          , _matchup_t_opponent = val_ $ m ^. matchup_opponent
          , _matchup_t_win = val_ $ m ^. matchup_win
          , _matchup_t_home = val_ $ m ^. matchup_home
          , _matchup_t_teamScore = val_ $ m ^. matchup_teamScore
          , _matchup_t_oppScore = val_ $ m ^. matchup_oppScore
          }
      ]
    )
    onConflictDefault
    (Just id)

create
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Matchup
  -> m MatchupRow
create m = do
  conn <- ask
  runInsertReturning conn (insertMatchup m) singleRow

matchupExists
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m Bool
matchupExists matchupId = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query = pure $ exists_ $ selectMatchupBy _matchup_t_id (val_ matchupId)

selectMatchupBy
  :: HasSqlEqualityCheck Postgres a
  => (MatchupT (PgQExpr s) -> PgQExpr s a)
  -> PgQExpr s a
  -> Q Postgres AppDb s (MatchupT (PgQExpr s))
selectMatchupBy f a = do
  matchup <- all_ (appMatchups appDb)
  guard_ (f matchup ==. a)
  pure matchup

find
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m (Maybe MatchupRow)
find matchupId = do
  conn <- ask
  runSelect conn (select (selectMatchupBy _matchup_t_id (val_ matchupId))) maybeRow

