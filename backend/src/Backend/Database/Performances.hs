{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend.Database.Performances where

import           Common.Api.Performances.Performance
import           Control.Lens                    (view, (^.), _1, _2)
import           Control.Monad.Error.Class       (MonadError, throwError)
import           Control.Monad.Fail              (MonadFail)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader.Class      (MonadReader, ask)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Crypto.Scrypt                   (EncryptedPass (EncryptedPass, getEncryptedPass),
                                                  Pass (Pass), encryptPassIO', verifyPass')
import           Data.Functor                    (void)
import           Data.Functor.Compose            (Compose (Compose, getCompose))
import           Data.Int                        (Int32)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.Validation                 (Validation (Failure, Success), validation)
import           Database.Beam.Postgres.Extended (HasSqlEqualityCheck, Nullable, PgExpressionSyntax,
                                                  PgInsertReturning, PgQExpr, Postgres, Postgres,
                                                  PgUpdateReturning, Q, aggregate_, all_, default_, delete,
                                                  exists_, group_, guard_, insertExpressions, insertReturning,
                                                  just_, leftJoin_, onConflictDefault, pgBoolOr, primaryKey,
                                                  references_, runDelete, runInsertReturning, runSelect,
                                                  runUpdateReturning, select, updateReturning, val_, (&&.),
                                                  (<-.), (==.))
import           Database.PostgreSQL.Simple      (Connection)

import           Backend.Database              (AppDb (appPerformances), QueryError,
                                                  appDb, maybeRow, singleRow)
import           Backend.Database.Performances.Performance
import           Backend.Validation            (ValidationErrors, requiredText)

insertPerformance
  :: Performance -> PgInsertReturning PerformanceRow
insertPerformance p
  = insertReturning
    (appPerformances appDb)
    (insertExpressions
      [ PerformanceT
          { _performance_t_playerName = val_ $ p ^. performance_player
          , _performance_t_matchupId = val_ $ p ^. performance_matchupId
          , _performance_t_status = val_ $ p ^. performance_status
          , _performance_t_points = val_ $ p ^. performance_points
          , _performance_t_offensiveRebounds = val_ $ p ^. performance_offensiveRebounds
          , _performance_t_defensiveRebounds = val_ $ p ^. performance_defensiveRebounds
          , _performance_t_assists = val_ $ p ^. performance_assists
          , _performance_t_steals = val_ $ p ^. performance_steals
          , _performance_t_blocks = val_ $ p ^. performance_blocks
          , _performance_t_turnovers = val_ $ p ^. performance_turnovers
          , _performance_t_fieldGoalsMade = val_ $ p ^. performance_fieldGoalsMade
          , _performance_t_fieldGoalsAttempted = val_ $ p ^. performance_fieldGoalsAttempted
          , _performance_t_freeThrowsMade = val_ $ p ^. performance_freeThrowsMade
          , _performance_t_freeThrowsAttempted = val_ $ p ^. performance_freeThrowsAttempted
          , _performance_t_threePointersMade = val_ $ p ^. performance_threePointersMade
          , _performance_t_threePointersAttempted = val_ $ p ^. performance_threePointersAttempted
          , _performance_t_personalFouls = val_ $ p ^. performance_personalFouls
          , _performance_t_minutes = val_ $ p ^. performance_minutes
          , _performance_t_fic = val_ $ p ^. performance_fic
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
  => Performance
  -> m PerformanceRow
create p = do
  conn <- ask
  runInsertReturning conn (insertPerformance p) singleRow

performanceExists
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> Text
  -> m Bool
performanceExists matchupId playerName = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query = pure $ exists_ $ selectPerformanceBy matchupId playerName

selectPerformanceBy
  :: Text
  -> Text
  -> Q Postgres AppDb s (PerformanceT (PgQExpr s))
selectPerformanceBy matchupId playerName = do
  p <- all_ (appPerformances appDb)
  guard_ (p ^. performance_t_matchupId ==. val_ matchupId)
  guard_ (p ^. performance_t_playerName ==. val_ playerName)
  pure p

find
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> Text
  -> m (Maybe PerformanceRow)
find matchupId playerName = do
  conn <- ask
  runSelect conn (select (selectPerformanceBy matchupId playerName)) maybeRow
