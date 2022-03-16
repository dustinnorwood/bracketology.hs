{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
module Backend.Database.Teams where

import Prelude hiding (all)

import           Control.Lens
import           Control.Monad.Error.Class       (MonadError)
import           Control.Monad.Fail              (MonadFail)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class      (MonadReader, ask)
import           Control.Monad.Trans.Control     (MonadBaseControl)
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
                                                  updateReturning, val_, (&&.), (<-.), (==.))

import           Database.PostgreSQL.Simple      (Connection)


import           Backend.Database                (AppDb (..), QueryError (..), appDb,
                                                   maybeRow, rowList, singleRow)
import           Backend.Database.Teams.Team
import           Common.Api.Teams.Team

insertTeam
  :: Team -> PgInsertReturning TeamRow
insertTeam team
  = insertReturning
    (appTeams appDb)
    (insertExpressions
      [ TeamT
          { _team_t_url   = val_ $ team ^. team_url
          , _team_t_name  = val_ $ team ^. team_name
          , _team_t_image = val_ $ team ^. team_image
          , _team_t_score = val_ $ team ^. team_score
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
  => Team
  -> m Team
create team = do
  conn <- ask
  inserted <-
    runInsertReturning
      conn
      (insertTeam team)
      singleRow
  unsafeFind $ inserted ^. team_t_name


teamRowToTeam :: TeamRow -> Team
teamRowToTeam (TeamT u n i s) = Team u n i s

find
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m (Maybe Team)
find name = do
  conn <- ask
  fmap teamRowToTeam <$>
    runSelect conn (select (selectTeam name)) maybeRow

unsafeFind
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m Team
unsafeFind name = do
  conn <- ask
  teamRowToTeam <$>
    runSelect conn (select (selectTeam name)) singleRow

updateTeamImage
  :: Text -> Text -> PgUpdateReturning TeamRow
updateTeamImage teamName teamImage
  = updateReturning
    (appTeams appDb)
    (\team -> _team_t_image team <-. val_ teamImage)
    ((val_ teamName ==.) . _team_t_name)
    id

updateImage
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> Text
  -> m Team
updateImage teamName teamImage = do
  conn <- ask
  updated <-
    runUpdateReturning
      conn
      (updateTeamImage teamName teamImage)
      singleRow
  unsafeFind teamName

type TeamRowQuery s = TeamT (PgQExpr s)

selectTeams :: Q Postgres AppDb s (TeamRowQuery s)
selectTeams = all_ (appTeams appDb)

selectFilteredTeams
  :: Integer
  -> Integer
  -> Q Postgres AppDb s (TeamRowQuery s)
selectFilteredTeams limit offset
  = orderBy_ (desc_ . _team_t_name)
    $ limit_ limit
    $ offset_ offset
    $ selectTeams

all
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Integer
  -> Integer
  -> m [Team]
all limit offset = do
  conn <- ask
  teams <-
    runSelect
      conn
      (select
         (selectFilteredTeams
            limit
            offset))
      rowList
  pure (teamRowToTeam <$> teams)

selectTeam
  :: Text
  -> Q Postgres AppDb s (TeamRowQuery s)
selectTeam name = do
  team <- selectTeams
  guard_ $ team ^. team_t_name ==. val_ name
  pure team

nameExists
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => Text
  -> m Bool
nameExists name = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query :: Q Postgres AppDb s (PgQExpr s Bool)
    query = pure $ exists_ $ do
      team <- all_ (appTeams appDb)
      guard_ (team ^. team_t_name ==. val_ name)
      pure team