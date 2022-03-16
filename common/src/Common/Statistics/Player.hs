{-# LANGUAGE OverloadedStrings #-}

module Common.Statistics.Player where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bool                   (bool)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Ordered            as O
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Read                   (readMaybe)
import           System.Random
import           Common.Api.Matchups.Matchup
import           Common.Api.Performances.Performance
import           Common.Api.Players.Player
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Probability
import           Common.Statistics.Util

playerPerformances :: TeamObject -> Map Text [Performance]
playerPerformances tObj = getPerformances tObj <$> tObj ^. players
  where getPerformances teamObj p = M.elems
                                  $ M.filterWithKey
                                  (\(pName,_) v -> pName == p ^. player_name)
                                  (teamObj ^. performances)

fieldGoalsAttemptedForPlayer :: [Performance] -> Integer
fieldGoalsAttemptedForPlayer = sum . fmap _performance_fieldGoalsAttempted

fieldGoalsAttemptedByPlayer :: TeamObject -> Map Text Integer
fieldGoalsAttemptedByPlayer = fmap fieldGoalsAttemptedForPlayer . playerPerformances

fieldGoalsMadeForPlayer :: [Performance] -> Integer
fieldGoalsMadeForPlayer = sum . fmap _performance_fieldGoalsMade

fieldGoalsMadeByPlayer :: TeamObject -> Map Text Integer
fieldGoalsMadeByPlayer = fmap fieldGoalsMadeForPlayer . playerPerformances

fieldGoalsMadeByGameForPlayer :: [Performance] -> [Double]
fieldGoalsMadeByGameForPlayer = fmap (toDouble . _performance_fieldGoalsMade)

fieldGoalsMadeByGameByPlayer :: TeamObject -> Map Text [Double]
fieldGoalsMadeByGameByPlayer = fmap fieldGoalsMadeByGameForPlayer . playerPerformances

freeThrowsAttemptedForPlayer :: [Performance] -> Integer
freeThrowsAttemptedForPlayer = sum . fmap _performance_freeThrowsAttempted

freeThrowsAttemptedByPlayer :: TeamObject -> Map Text Integer
freeThrowsAttemptedByPlayer = fmap freeThrowsAttemptedForPlayer . playerPerformances

freeThrowsMadeForPlayer :: [Performance] -> Integer
freeThrowsMadeForPlayer = sum . fmap _performance_freeThrowsMade

freeThrowsMadeByPlayer :: TeamObject -> Map Text Integer
freeThrowsMadeByPlayer = fmap freeThrowsMadeForPlayer . playerPerformances

freeThrowsMadeByGameForPlayer :: [Performance] -> [Double]
freeThrowsMadeByGameForPlayer = fmap (toDouble . _performance_freeThrowsMade)

freeThrowsMadeByGameByPlayer :: TeamObject -> Map Text [Double]
freeThrowsMadeByGameByPlayer = fmap freeThrowsMadeByGameForPlayer . playerPerformances

threePointersAttemptedForPlayer :: [Performance] -> Integer
threePointersAttemptedForPlayer = sum . fmap _performance_threePointersAttempted

threePointersAttemptedByPlayer :: TeamObject -> Map Text Integer
threePointersAttemptedByPlayer = fmap threePointersAttemptedForPlayer . playerPerformances

threePointersMadeForPlayer :: [Performance] -> Integer
threePointersMadeForPlayer = sum . fmap _performance_threePointersMade

threePointersMadeByPlayer :: TeamObject -> Map Text Integer
threePointersMadeByPlayer = fmap threePointersMadeForPlayer . playerPerformances

threePointersMadeByGameForPlayer :: [Performance] -> [Double]
threePointersMadeByGameForPlayer = fmap (toDouble . _performance_threePointersMade)

threePointersMadeByGameByPlayer :: TeamObject -> Map Text [Double]
threePointersMadeByGameByPlayer = fmap threePointersMadeByGameForPlayer . playerPerformances

assistsForPlayer :: [Performance] -> Integer
assistsForPlayer = sum . fmap _performance_assists

assistsByPlayer :: TeamObject -> Map Text Integer
assistsByPlayer = fmap assistsForPlayer . playerPerformances

stealsForPlayer :: [Performance] -> Integer
stealsForPlayer = sum . fmap _performance_steals

stealsByPlayer :: TeamObject -> Map Text Integer
stealsByPlayer = fmap stealsForPlayer . playerPerformances

blocksForPlayer :: [Performance] -> Integer
blocksForPlayer = sum . fmap _performance_blocks

blocksByPlayer :: TeamObject -> Map Text Integer
blocksByPlayer = fmap blocksForPlayer . playerPerformances

offensiveReboundsForPlayer :: [Performance] -> Integer
offensiveReboundsForPlayer = sum . fmap _performance_offensiveRebounds

offensiveReboundsByPlayer :: TeamObject -> Map Text Integer
offensiveReboundsByPlayer = fmap offensiveReboundsForPlayer . playerPerformances

defensiveReboundsForPlayer :: [Performance] -> Integer
defensiveReboundsForPlayer = sum . fmap _performance_defensiveRebounds

defensiveReboundsByPlayer :: TeamObject -> Map Text Integer
defensiveReboundsByPlayer = fmap defensiveReboundsForPlayer . playerPerformances

-- TODO: Use foldl library
threePointTakenPercentageForPlayer :: [Performance] -> Double
threePointTakenPercentageForPlayer ps =
  let threes = threePointersAttemptedForPlayer ps
      fgs = fieldGoalsAttemptedForPlayer ps
   in fromInteger threes / fromInteger fgs

threePointTakenPercentageByPlayer :: TeamObject -> Map Text Double
threePointTakenPercentageByPlayer = fmap threePointTakenPercentageForPlayer . playerPerformances

fieldGoalPercentageForPlayer :: [Performance] -> Double
fieldGoalPercentageForPlayer ps =
  let fgms = fieldGoalsMadeForPlayer ps
      fgas = fieldGoalsAttemptedForPlayer ps
   in fromInteger fgms / fromInteger fgas

fieldGoalPercentageByPlayer :: TeamObject -> Map Text Double
fieldGoalPercentageByPlayer = fmap fieldGoalPercentageForPlayer . playerPerformances

freeThrowPercentageForPlayer :: [Performance] -> Double
freeThrowPercentageForPlayer ps =
  let fgms = freeThrowsMadeForPlayer ps
      fgas = freeThrowsAttemptedForPlayer ps
   in fromInteger fgms / fromInteger fgas

freeThrowPercentageByPlayer :: TeamObject -> Map Text Double
freeThrowPercentageByPlayer = fmap freeThrowPercentageForPlayer . playerPerformances

threePointPercentageForPlayer :: [Performance] -> Double
threePointPercentageForPlayer ps =
  let fgms = threePointersMadeForPlayer ps
      fgas = threePointersAttemptedForPlayer ps
   in fromInteger fgms / fromInteger fgas

threePointPercentageByPlayer :: TeamObject -> Map Text Double
threePointPercentageByPlayer = fmap threePointPercentageForPlayer . playerPerformances

foulsForPlayer :: [Performance] -> Integer
foulsForPlayer = sum . fmap _performance_personalFouls

foulsByPlayer :: TeamObject -> Map Text Integer
foulsByPlayer = fmap foulsForPlayer . playerPerformances

minutesForPlayer :: [Performance] -> Double
minutesForPlayer = sum . fmap (fromMaybe 0.0 . readMaybe . T.unpack . _performance_minutes)

minutesByPlayer :: TeamObject -> Map Text Double
minutesByPlayer = fmap minutesForPlayer . playerPerformances

minutesByGameForPlayer :: [Performance] -> [Double]
minutesByGameForPlayer = fmap (fromMaybe 0.0 . readMaybe . T.unpack . _performance_minutes)

minutesByGameByPlayer :: TeamObject -> Map Text [Double]
minutesByGameByPlayer = fmap minutesByGameForPlayer . playerPerformances

startsForPlayer :: [Performance] -> Integer
startsForPlayer = toInteger . length . filter ((== "Starter") . _performance_status)

startsByPlayer :: TeamObject -> Map Text Integer
startsByPlayer = fmap startsForPlayer . playerPerformances

benchStartsForPlayer :: [Performance] -> Integer
benchStartsForPlayer = toInteger . length . filter ((== "Bench") . _performance_status)

benchStartsByPlayer :: TeamObject -> Map Text Integer
benchStartsByPlayer = fmap benchStartsForPlayer . playerPerformances

starterPointsForPlayer :: [Performance] -> Integer
starterPointsForPlayer = sum . fmap _performance_points . filter ((== "Starter") . _performance_status)

starterPointsByPlayer :: TeamObject -> Map Text Integer
starterPointsByPlayer = fmap starterPointsForPlayer . playerPerformances

benchPointsForPlayer :: [Performance] -> Integer
benchPointsForPlayer = sum . fmap _performance_points . filter ((== "Bench") . _performance_status)

benchPointsByPlayer :: TeamObject -> Map Text Integer
benchPointsByPlayer = fmap benchPointsForPlayer . playerPerformances

turnoversForPlayer :: [Performance] -> Integer
turnoversForPlayer = sum . fmap _performance_turnovers

turnoversByPlayer :: TeamObject -> Map Text Integer
turnoversByPlayer = fmap turnoversForPlayer . playerPerformances
