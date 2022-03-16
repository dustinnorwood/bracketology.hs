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

module Common.Api.Teams.TeamRankings where

import Control.Lens hiding (Context, (??), noneOf)

import           Control.Monad.FT.Alter
import           Control.Monad.Except             (ExceptT (ExceptT), runExceptT, withExceptT)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (asks, ReaderT, runReaderT)
import           Control.Monad.State
import           Data.Functor                     (($>))
import           Data.IORef
import qualified Data.Map.Ordered                 as O
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Prelude                          hiding (lookup)
import           Text.Parsec


-- import           Backend.Claim                     (Claim (Claim), deriveToken)
import           Common.Api                        as Api
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Api.Players.Player
import           Common.Api.Matchups.Matchup
import           Common.Api.Performances.Performance
import           Common.Route

getTeamRawScores :: Map Text TeamObject -> Map Text TeamObject
getTeamRawScores teams = getTeamRawScore teams <$> teams

getTeamRawScore :: Map Text TeamObject -> TeamObject -> TeamObject
getTeamRawScore m teamObj =
  let matchups' = O.assocs $ teamObj ^. matchups
      newScore = fst $ foldr rawScore (0.0, 1.0) $ snd <$> matchups'
   in (team . team_score .~ newScore) teamObj
  where rawScore Matchup{..} (s, fade) = case M.lookup _matchup_opponent m of
          Nothing -> (s, fade * 1.0)
          Just opp ->
            let oppWinPercentage = team_winPercentage . map snd $ O.assocs $ opp ^. matchups
                scoreDiff = fromInteger (_matchup_teamScore - _matchup_oppScore) / 35
                toleranceAdjustment = if _matchup_win then 0 else -1.0
                newS = s + fade * (oppWinPercentage + scoreDiff + toleranceAdjustment)
             in (newS, fade * 1.0)

adjustTeamScores :: Map Text TeamObject -> Map Text TeamObject
adjustTeamScores teams =
  let maxScore = maximum $ (^. team . team_score) <$> M.elems teams
      minScore = minimum $ (^. team . team_score) <$> M.elems teams
      teams' = adjustTeamScore teams maxScore minScore <$> teams
      maxScore' = maximum $ (^. team . team_score) <$> M.elems teams'
      minScore' = minimum $ (^. team . team_score) <$> M.elems teams'
      range' = maxScore' - minScore'
    in (team . team_score %~ (\s -> (s - minScore') / range')) <$> teams'


adjustTeamScore :: Map Text TeamObject -> Double -> Double -> TeamObject -> TeamObject
adjustTeamScore m maxScore minScore teamObj =
  let matchups' = O.assocs $ teamObj ^. matchups
      newScore = fst $ foldr rawScore (0.0, 1.0) $ snd <$> matchups'
   in (team . team_score .~ newScore) teamObj
  where rawScore Matchup{..} (s, fade) = case M.lookup _matchup_opponent m of
          Nothing -> (s, fade * 0.95)
          Just opp ->
            let oppScore = opp ^. team . team_score
                scoreDiff = (oppScore - minScore) / (maxScore - minScore)
                toleranceAdjustment = if _matchup_win then 0 else -1.0
                newS = s + fade * (scoreDiff + toleranceAdjustment)
             in (newS, fade * 0.95)

whitenTeamScores :: Map Text TeamObject -> Map Text TeamObject  
whitenTeamScores teams =
  let maxScore = maximum $ (^. team . team_score) <$> M.elems teams
      minScore = minimum $ (^. team . team_score) <$> M.elems teams
   in whitenTeamScore teams maxScore minScore <$> teams

whitenTeamScore :: Map Text TeamObject -> Double -> Double -> TeamObject -> TeamObject
whitenTeamScore m maxScore minScore teamObj =
  let s = teamObj ^. team . team_score
      s' = s - minScore
      s'' = s' / (maxScore - minScore)
      s''' = s'' + 1.0
      s'''' = s''' / 2
      s''''' = s'''' * s''''
   in (team . team_score .~ s''''') teamObj