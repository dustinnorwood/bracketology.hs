{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Backend.Monad
  ( module Backend.Monad
  ) where

import Control.Lens hiding (Context, (??))

import           Control.Monad.FT.Alter
import           Control.Monad.Except             (ExceptT (ExceptT), runExceptT, withExceptT)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (asks, ReaderT, runReaderT)
import           Control.Monad.State
import           Data.Function                    (on)
import           Data.IORef
import           Data.List                        (sortBy)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (fromMaybe)
import           Data.Ord                         (Down(..))
import           Data.Pool                        (Pool, createPool, withResource)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Database.PostgreSQL.Simple       (Connection, close)
import "servant-snap" Servant                     ((:<|>) ((:<|>)), Server)
import           Snap.Core                        (Snap)
import           Prelude                          hiding (lookup)


-- import           Backend.Claim                     (Claim (Claim), deriveToken)
import qualified Backend.Database                    as Database
import qualified Backend.Database.Teams              as DBTeams 
import           Backend.Errors
import           Common.Api                          as Api
import           Common.Api.Performances.Performance (Performance(..))
import           Common.Api.Players.Player           (Player(..))
import           Common.Api.Teams.Team               (Team(..))
import           Common.Api.Teams.TeamObject
import           Common.Route

data AppServerEnv = AppServerEnv
  { _dbPool      :: Pool Connection
  }
makeLenses ''AppServerEnv

data AppMemServerEnv = AppMemServerEnv
  { _teams      :: IORef (Map Text TeamObject)
  }
makeLenses ''AppMemServerEnv

type AppServerM   = ReaderT AppServerEnv Snap
type AppMemServerM = ReaderT AppMemServerEnv Snap
type AppServerDbM = AppErrorsT (ReaderT Connection IO) --Concrete type for DB queries
type AppServerContext = '[]

newtype Prefixed a = Prefixed { unPrefixed :: a }

instance Selectable (Map Text Team) GetTeams AppMemServerM where
  select (Windowed l o _) = do
    pkgsRef <- asks _teams
    teamList <- fmap (sortBy (compare `on` (negate . (^. _2 . team . team_score))) . M.toList) . liftIO $ readIORef pkgsRef
    let windowed = take (fromInteger $ fromMaybe 20 l) $ drop (fromInteger $ fromMaybe 0 o) teamList
    pure $ if null windowed
             then Nothing
             else Just $ M.fromList $ fmap _team <$> windowed

instance Selectable (Prefixed (Map Text Team)) SearchTeams AppMemServerM where
  select (Windowed _ _ (SearchTeamsParams "")) = pure . Just $ Prefixed M.empty
  select (Windowed l o (SearchTeamsParams t)) = do
    pkgsRef <- asks _teams
    teamList <- fmap M.toList . liftIO $ readIORef pkgsRef
    let windowed = take (fromInteger $ fromMaybe 20 l) $ drop (fromInteger $ fromMaybe 0 o) $ filter (T.isPrefixOf (T.toLower t) . T.toLower . fst) teamList
    pure $ if null windowed
             then Nothing
             else Just . Prefixed $ M.fromList $ fmap _team <$> windowed

instance Selectable TeamObject TeamName AppMemServerM where
  select (TeamName teamName) = do
    pkgsRef <- asks _teams
    fmap (M.lookup teamName) . liftIO $ readIORef pkgsRef

instance Selectable Team Text AppMemServerM where
  select slug = do
    pkgsRef <- asks _teams
    teamList <- liftIO $ readIORef pkgsRef
    pure . fmap _team $ M.lookup slug teamList

instance Selectable (Player, [Performance]) (TeamName, PlayerName) AppMemServerM where
  select (TeamName teamName, PlayerName playerName) = do
    pkgsRef <- asks _teams
    teamList <- liftIO $ readIORef pkgsRef
    let mTeam = M.lookup teamName teamList
        mPlayerPerfs = join $ mTeam <&> \TeamObject{..} ->
          let mPlayer = M.lookup playerName _players
              perfs = M.elems $ M.filterWithKey (\(_,p) _ -> p == playerName) _performances
           in flip (,) perfs <$> mPlayer
    pure mPlayerPerfs


-- instance (Lookupable (Map Text Team) GetTeams) AppServerM where
--   lookup (Windowed l o (GetTeamsParams t f)) = runAppErrorsT $ do
--       runDatabase $ do
--         -- currPlayerMay <- optionallyLoadAuthorizedPlayer authRes
--         let currPlayerMay = Just dummyPlayer
--         ApiTeams.fromList <$>
--           (DBTeams.all
--            (primaryKey <$> currPlayerMay)
--            (fromMaybe 20 l)
--            (fromMaybe 0 o)
--            (Set.fromList $ maybeToList t)
--            (Set.fromList $ maybeToList f))

runAppServerM :: AppServerEnv -> AppServerM a -> Snap a
runAppServerM e = flip runReaderT e

runAppMemServerM :: AppMemServerEnv -> AppMemServerM a -> Snap a
runAppMemServerM e = flip runReaderT e

mkEnv :: MonadIO m => Text -> m AppServerEnv
mkEnv dbConnStr = do
  p <- liftIO $ createPool (Database.openAppDb (encodeUtf8 dbConnStr)) close 1 10 8
  pure $ AppServerEnv p

server :: Server Api AppServerContext AppServerM
server = teamsServer

teamsServer :: Server TeamsApi AppServerContext AppServerM
teamsServer = listTeamsServer :<|> teamServer
  where
    listTeamsServer limit offset = runAppErrorsT $ do
      runDatabase $ do
        DBTeams.all
          (fromMaybe 20 limit)
          (fromMaybe 0 offset)
    teamServer name = getTeamServer
      where
        getTeamServer = runAppErrorsT $ do
          team <- runDatabase loadTeam
          pure $ Namespace team

        loadTeam = do
          teamMay  <- liftQuery $ DBTeams.find name
          teamMay ?? notFound ("Team(" <> name <> ")")

-- We put ExceptT on top so it is easier to use (!?) and friends
runDatabase :: AppServerDbM a -> AppErrorsT AppServerM a
runDatabase m = do
  p <- view dbPool
  -- TODO: There should probably be a transaction here ;)
  ExceptT . liftIO . withResource p $ runReaderT (runExceptT m)

liftQuery :: ExceptT Database.QueryError (ReaderT Connection IO) a -> AppErrorsT (ReaderT Connection IO) a
liftQuery = withExceptT (internalServerErrorShow "QueryError")