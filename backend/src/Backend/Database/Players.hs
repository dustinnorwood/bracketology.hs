{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend.Database.Players where

import           Common.Api.Players.Player
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

import           Backend.Database              (AppDb (appPlayers), QueryError,
                                                  appDb, maybeRow, singleRow)
import           Backend.Database.Players.Player

insertPlayer
  :: Player -> PgInsertReturning PlayerRow
insertPlayer p
  = insertReturning
    (appPlayers appDb)
    (insertExpressions
      [ PlayerT
          { _player_t_name = val_ $ p ^. player_name
          , _player_t_teamName = val_ $ p ^. player_team_name
          , _player_t_position = val_ $ p ^. player_position
          , _player_t_gamesPlayed = val_ $ p ^. player_gamesPlayed
          , _player_t_height = val_ $ p ^. player_height
          , _player_t_weight = val_ $ p ^. player_weight
          , _player_t_number = val_ $ p ^. player_number
          , _player_t_class = val_ $ p ^. player_class
          , _player_t_birthDate = val_ $ p ^. player_birthDate
          , _player_t_birthCity = val_ $ p ^. player_birthCity
          , _player_t_nationality = val_ $ p ^. player_nationality
          , _player_t_highSchool = val_ $ p ^. player_highSchool
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
  => Player
  -> m PlayerRow
create reg = do
  conn <- ask
  runInsertReturning conn (insertPlayer reg) singleRow

playerExists
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m Bool
playerExists name = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query = pure $ exists_ $ selectPlayerBy _player_t_name (val_ name)

selectPlayerBy
  :: HasSqlEqualityCheck Postgres a
  => (PlayerT (PgQExpr s) -> PgQExpr s a)
  -> PgQExpr s a
  -> Q Postgres AppDb s (PlayerT (PgQExpr s))
selectPlayerBy f a = do
  player <- all_ (appPlayers appDb)
  guard_ (f player ==. a)
  pure player

find
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m (Maybe PlayerRow)
find playerName = do
  conn <- ask
  runSelect conn (select (selectPlayerBy _player_t_name (val_ playerName))) maybeRow