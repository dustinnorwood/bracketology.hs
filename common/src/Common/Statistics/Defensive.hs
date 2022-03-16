{-# LANGUAGE OverloadedStrings #-}

module Common.Statistics.Defensive where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bool                   (bool)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Ordered            as O
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.Random
import           Common.Api.Matchups.Matchup
import           Common.Api.Performances.Performance
import           Common.Api.Players.Player
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Probability
import           Common.Statistics.Offensive
import           Common.Statistics.Util

pointsAllowedPerGame :: [Matchup] -> Double
pointsAllowedPerGame [] = 0.0
pointsAllowedPerGame xs =
  let num = sum $ pointsAllowedByGame xs
      den = length xs
   in toDouble num / toDouble den

pointsAllowedByGame :: [Matchup] -> [Integer]
pointsAllowedByGame = map _matchup_oppScore

matchupOpponents :: Map Text TeamObject -> TeamObject -> [Maybe TeamObject]
matchupOpponents tms tObj = getOpponents tms . snd <$> O.assocs (tObj ^. matchups)
  where getOpponents teams m = M.lookup (m ^. matchup_opponent) teams

opponentMatchupPerformances :: Map Text TeamObject -> TeamObject -> [[Performance]]
opponentMatchupPerformances tms tObj = getPerformances tms tObj . snd <$> O.assocs (tObj ^. matchups)
  where getPerformances teams teamObj m = M.elems
          $ M.filterWithKey
          (\(_,mId) v -> mId == getOppMatchupId
                                  (teamObj ^. team . team_name)
                                  m)
          (maybe M.empty (^. performances) $ opponent teams m)
        opponent teams m = M.lookup (m ^. matchup_opponent) teams
  
opponentPointsPerGameByGame :: Map Text TeamObject -> TeamObject -> [Double]
opponentPointsPerGameByGame teamMap teamObj = 
   let opponents = matchupOpponents teamMap teamObj
    in maybe 0.0 pointsPerGame <$> opponents
  
opponentFieldGoalsAttemptedByGame :: Map Text TeamObject -> TeamObject -> [Integer]
opponentFieldGoalsAttemptedByGame teamMap teamObj = 
   let opponents = opponentMatchupPerformances teamMap teamObj
    in fieldGoalsAttemptedInGame <$> opponents
  
opponentStealsByGame :: Map Text TeamObject -> TeamObject -> [Integer]
opponentStealsByGame teamMap teamObj = 
   let opponents = opponentMatchupPerformances teamMap teamObj
    in stealsInGame <$> opponents
  
opponentBlocksByGame :: Map Text TeamObject -> TeamObject -> [Integer]
opponentBlocksByGame teamMap teamObj = 
   let opponents = opponentMatchupPerformances teamMap teamObj
    in blocksInGame <$> opponents
  
opponentFoulsByGame :: Map Text TeamObject -> TeamObject -> [Integer]
opponentFoulsByGame teamMap teamObj = 
   let opponents = opponentMatchupPerformances teamMap teamObj
    in foulsInGame <$> opponents