{-# LANGUAGE OverloadedStrings #-}

module Common.Statistics.Offensive where

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
import           Common.Statistics.Util

matchupPerformances :: TeamObject -> [[Performance]]
matchupPerformances tObj = getPerformances tObj . snd <$> O.assocs (tObj ^. matchups)
  where getPerformances teamObj m = M.elems
                                  $ M.filterWithKey
                                  (\(_,mId) v -> mId == m ^. matchup_id)
                                  (teamObj ^. performances)

fieldGoalsAttemptedInGame :: [Performance] -> Integer
fieldGoalsAttemptedInGame = sum . map _performance_fieldGoalsAttempted

fieldGoalsAttemptedByGame :: TeamObject -> [Integer]
fieldGoalsAttemptedByGame = map fieldGoalsAttemptedInGame . matchupPerformances

fieldGoalsMadeInGame :: [Performance] -> Integer
fieldGoalsMadeInGame = sum . map _performance_fieldGoalsMade

fieldGoalsMadeByGame :: TeamObject -> [Integer]
fieldGoalsMadeByGame = map fieldGoalsMadeInGame . matchupPerformances

freeThrowsAttemptedInGame :: [Performance] -> Integer
freeThrowsAttemptedInGame = sum . map _performance_freeThrowsAttempted

freeThrowsAttemptedByGame :: TeamObject -> [Integer]
freeThrowsAttemptedByGame = map freeThrowsAttemptedInGame . matchupPerformances

freeThrowsMadeInGame :: [Performance] -> Integer
freeThrowsMadeInGame = sum . map _performance_freeThrowsMade

freeThrowsMadeByGame :: TeamObject -> [Integer]
freeThrowsMadeByGame = map freeThrowsMadeInGame . matchupPerformances

threePointersAttemptedInGame :: [Performance] -> Integer
threePointersAttemptedInGame = sum . map _performance_threePointersAttempted

threePointersAttemptedByGame :: TeamObject -> [Integer]
threePointersAttemptedByGame = map threePointersAttemptedInGame . matchupPerformances

threePointersMadeInGame :: [Performance] -> Integer
threePointersMadeInGame = sum . map _performance_threePointersMade

threePointersMadeByGame :: TeamObject -> [Integer]
threePointersMadeByGame = map threePointersMadeInGame . matchupPerformances

assistsInGame :: [Performance] -> Integer
assistsInGame = sum . map _performance_assists

assistsByGame :: TeamObject -> [Integer]
assistsByGame = map assistsInGame . matchupPerformances

stealsInGame :: [Performance] -> Integer
stealsInGame = sum . map _performance_steals

stealsByGame :: TeamObject -> [Integer]
stealsByGame = map stealsInGame . matchupPerformances

blocksInGame :: [Performance] -> Integer
blocksInGame = sum . map _performance_blocks

blocksByGame :: TeamObject -> [Integer]
blocksByGame = map blocksInGame . matchupPerformances

offensiveReboundsInGame :: [Performance] -> Integer
offensiveReboundsInGame = sum . map _performance_offensiveRebounds

offensiveReboundsByGame :: TeamObject -> [Integer]
offensiveReboundsByGame = map offensiveReboundsInGame . matchupPerformances

defensiveReboundsInGame :: [Performance] -> Integer
defensiveReboundsInGame = sum . map _performance_defensiveRebounds

defensiveReboundsByGame :: TeamObject -> [Integer]
defensiveReboundsByGame = map defensiveReboundsInGame . matchupPerformances

turnoversInGame :: [Performance] -> Integer
turnoversInGame = sum . map _performance_turnovers

turnoversByGame :: TeamObject -> [Integer]
turnoversByGame = map turnoversInGame . matchupPerformances

-- TODO: Use foldl library
threePointTakenPercentageInGame :: [Performance] -> Double
threePointTakenPercentageInGame ps =
  let threes = threePointersAttemptedInGame ps
      fgs = fieldGoalsAttemptedInGame ps
   in fromInteger threes / fromInteger fgs

threePointTakenPercentageByGame :: TeamObject -> [Double]
threePointTakenPercentageByGame = map threePointTakenPercentageInGame . matchupPerformances

fieldGoalPercentageInGame :: [Performance] -> Double
fieldGoalPercentageInGame ps =
  let fgms = fieldGoalsMadeInGame ps
      fgas = fieldGoalsAttemptedInGame ps
   in fromInteger fgms / fromInteger fgas

fieldGoalPercentageByGame :: TeamObject -> [Double]
fieldGoalPercentageByGame = map fieldGoalPercentageInGame . matchupPerformances

freeThrowPercentageInGame :: [Performance] -> Double
freeThrowPercentageInGame ps =
  let fgms = freeThrowsMadeInGame ps
      fgas = freeThrowsAttemptedInGame ps
   in fromInteger fgms / fromInteger fgas

freeThrowPercentageByGame :: TeamObject -> [Double]
freeThrowPercentageByGame = map freeThrowPercentageInGame . matchupPerformances

threePointPercentageInGame :: [Performance] -> Double
threePointPercentageInGame ps =
  let fgms = threePointersMadeInGame ps
      fgas = threePointersAttemptedInGame ps
   in fromInteger fgms / fromInteger fgas

threePointPercentageByGame :: TeamObject -> [Double]
threePointPercentageByGame = map threePointPercentageInGame . matchupPerformances

foulsInGame :: [Performance] -> Integer
foulsInGame = sum . map _performance_personalFouls

foulsByGame :: TeamObject -> [Integer]
foulsByGame = map foulsInGame . matchupPerformances

pointsPerGame :: TeamObject -> Double
pointsPerGame = average . map (fromInteger . _matchup_teamScore . snd) . O.assocs . _matchups

starterPointsInGame :: [Performance] -> Integer
starterPointsInGame = sum . map _performance_points . filter ((== "Starter") . _performance_status)

starterPointsByGame :: TeamObject -> [Integer]
starterPointsByGame = map starterPointsInGame . matchupPerformances

benchPointsInGame :: [Performance] -> Integer
benchPointsInGame = sum . map _performance_points . filter ((== "Bench") . _performance_status)

benchPointsByGame :: TeamObject -> [Integer]
benchPointsByGame = map benchPointsInGame . matchupPerformances

teamFieldGoalPercentage :: TeamObject -> Double
teamFieldGoalPercentage teamObj =
  let ps = M.elems $ teamObj ^. performances
      fgms = sum . map _performance_fieldGoalsMade $ ps
      fgas = sum . map _performance_fieldGoalsAttempted $ ps
   in fromInteger fgms / fromInteger fgas

teamFreeThrowPercentage :: TeamObject -> Double
teamFreeThrowPercentage teamObj =
  let ps = M.elems $ teamObj ^. performances
      fgms = sum . map _performance_freeThrowsMade $ ps
      fgas = sum . map _performance_freeThrowsAttempted $ ps
   in fromInteger fgms / fromInteger fgas

teamThreePointPercentage :: TeamObject -> Double
teamThreePointPercentage teamObj =
  let ps = M.elems $ teamObj ^. performances
      fgms = sum . map _performance_threePointersMade $ ps
      fgas = sum . map _performance_threePointersAttempted $ ps
   in fromInteger fgms / fromInteger fgas

-- offensivePpgRatioInGame :: Map Text Team -> Matchup [Performance]) -> Double[]
-- offensivePpgRatioInGame

homeAdvantage :: ToDouble a => (TeamObject -> [a]) -> TeamObject -> Double
homeAdvantage stat teamObj =
  let teamStat = stat teamObj
      games = snd <$> O.assocs (teamObj ^. matchups)
      aways = filter (not . _matchup_home . fst) $ zip games teamStat
      homes = filter (_matchup_home . fst) $ zip games teamStat
      num = average $ toDouble . snd <$> homes
      den = average $ toDouble . snd <$> aways
   in if den == 0.0 then 0.0 else num / den


removeHomeAdvantage :: ToDouble a => (TeamObject -> [a]) -> TeamObject -> [Double]
removeHomeAdvantage stat teamObj =
  let games = snd <$> O.assocs (teamObj ^. matchups)
      adv = homeAdvantage stat teamObj
      homes = map (bool 0.0 1.0 . _matchup_home) games
      aways = map (bool 1.0 0.0 . _matchup_home) games
      divd = homes `divideDouble` adv
      addd = divd `addDoubles` aways
   in (toDouble <$> stat teamObj) `multiplyDoubles` addd