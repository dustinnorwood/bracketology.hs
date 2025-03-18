{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Common.Simulation.Details where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Aeson                  (ToJSON, FromJSON)
import           Data.Bool                   (bool)
import           Data.Function               (on)
import           Data.List                   (sortBy)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Ordered            as O
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Generics
import           System.Random
import           Common.Api.Players.Player
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Probability
import           Common.Statistics.Defensive
import           Common.Statistics.Offensive
import           Common.Statistics.Player
import           Common.Statistics.Util

data PlayerProbability = PP
  { _pp_player                :: Player
  , _pp_freeThrowProbability  :: Double
  , _pp_freeThrowPercentage   :: Double
  , _pp_fieldGoalProbability  :: Double
  , _pp_fieldGoalPercentage   :: Double
  , _pp_threePointProbability :: Double
  , _pp_threePointPercentage  :: Double
  , _pp_assistPercentage      :: Double
  , _pp_turnoverPercentage    :: Double
  , _pp_onCourtProbability    :: Double
  , _pp_startProbability      :: Double
  } deriving (Eq, Show)
makeLenses ''PlayerProbability

deriving instance Generic PlayerProbability
deriving instance ToJSON PlayerProbability
deriving instance FromJSON PlayerProbability

type TeamSimulationOutcome = ()

data TeamSimulationDetails = TSD
  { _tsd_teamObj                       :: TeamObject
  , _tsd_fieldGoalPercentage           :: Double
  , _tsd_fieldGoalsTaken               :: Double
  , _tsd_freeThrowPercentage           :: Double
  , _tsd_freeThrowsTaken               :: Double
  , _tsd_threePointPercentage          :: Double
  , _tsd_threePointsTaken              :: Double
  , _tsd_offensiveReboundPercentage    :: Double
  , _tsd_defensiveReboundPercentage    :: Double
  , _tsd_offensiveTurnoverPercentage   :: Double
  , _tsd_stealPercentage               :: Double
  , _tsd_stolenPercentage              :: Double
  , _tsd_blockPercentage               :: Double
  , _tsd_blockedPercentage             :: Double
  , _tsd_foulPercentage                :: Double
  , _tsd_fouledPercentage              :: Double
  , _tsd_playerProbabilities           :: Map Text PlayerProbability
  } deriving (Eq, Show)
makeLenses ''TeamSimulationDetails

deriving instance Generic TeamSimulationDetails
deriving instance ToJSON TeamSimulationDetails
deriving instance FromJSON TeamSimulationDetails

playerStat :: ToDouble a => (TeamObject -> Map Text a) -> Text -> TeamObject -> Double
playerStat stat playerName = maybe 0.0 toDouble . M.lookup playerName . stat

createTeamSimulationDetails :: Map Text TeamObject -> TeamObject -> TeamSimulationDetails
createTeamSimulationDetails teams teamObj =
  let oppfg = toDouble <$> opponentFieldGoalsAttemptedByGame teams teamObj
      fg = fieldGoalsAttemptedByGame teamObj
      fgd = toDouble <$> fg
      fgs = sum fg
      ft = freeThrowsAttemptedByGame teamObj
      fts = sum ft
      s = stealsByGame teamObj
      sd = toDouble <$> s
      b = blocksByGame teamObj
      bd = toDouble <$> b
      pps = flip M.map (teamObj ^. players) $ \p@Player{..} ->
        let fta = playerStat freeThrowsAttemptedByPlayer _player_name teamObj
            ftm = playerStat freeThrowsMadeByPlayer _player_name teamObj
            fga = playerStat fieldGoalsAttemptedByPlayer _player_name teamObj
            fgm = playerStat fieldGoalsMadeByPlayer _player_name teamObj
            tpa = playerStat threePointersAttemptedByPlayer _player_name teamObj 
            tpm = playerStat threePointersMadeByPlayer _player_name teamObj
            ast = playerStat assistsByPlayer _player_name teamObj
            tos = playerStat turnoversByPlayer _player_name teamObj
            mCount = O.size $ teamObj ^. matchups
         in PP
              { _pp_player = p
              , _pp_freeThrowProbability  = if fts == 0 then 0.0 else fta / fromIntegral fts
              , _pp_freeThrowPercentage   = if fta == 0 then 0.0 else ftm / fta
              , _pp_fieldGoalProbability  = if tpa + fga == 0 then 0.0 else fga / (tpa + fga)
              , _pp_fieldGoalPercentage   = if fga + fgm == 0.0 then 0.0 else fgm / fga
              , _pp_threePointProbability = if tpa + fga == 0 then 0.0 else tpa / (tpa + fga)
              , _pp_threePointPercentage  = if tpa + tpm == 0.0 then 0.0 else tpm / tpa
              , _pp_assistPercentage      = if ast + fromInteger fgs == 0.0 then 0.0 else ast / (ast + fga)
              , _pp_turnoverPercentage    = if ast + fromInteger fgs + tos == 0 then 0.0 else tos / (ast + fga + tos)
              , _pp_onCourtProbability    = min 1.0 $ (playerStat minutesByPlayer _player_name teamObj) * toDouble mCount / 40.0
              , _pp_startProbability      = if mCount == 0 then 0.0 else (playerStat startsByPlayer _player_name teamObj) / toDouble mCount
              }
   in TSD
        { _tsd_teamObj                       = teamObj
        , _tsd_fieldGoalPercentage           = average $ removeHomeAdvantage fieldGoalPercentageByGame teamObj
        , _tsd_fieldGoalsTaken               = average $ removeHomeAdvantage (const fg) teamObj
        , _tsd_freeThrowPercentage           = average $ removeHomeAdvantage freeThrowPercentageByGame teamObj
        , _tsd_freeThrowsTaken               = average $ removeHomeAdvantage (const ft) teamObj
        , _tsd_threePointPercentage          = average $ removeHomeAdvantage threePointPercentageByGame teamObj
        , _tsd_threePointsTaken              = average $ removeHomeAdvantage ((`divideDoubles` fgd) . map toDouble . threePointersAttemptedByGame) teamObj
        , _tsd_offensiveReboundPercentage    = average $ removeHomeAdvantage offensiveReboundsByGame teamObj
        , _tsd_defensiveReboundPercentage    = average $ removeHomeAdvantage defensiveReboundsByGame teamObj
        , _tsd_offensiveTurnoverPercentage   = average $ removeHomeAdvantage ((`divideDoubles` fgd) . map toDouble . turnoversByGame) teamObj
        , _tsd_stealPercentage                = average $ removeHomeAdvantage (const $ sd `divideDoubles` (sd `addDoubles` oppfg)) teamObj
        , _tsd_stolenPercentage               = average $ removeHomeAdvantage ((`divideDoubles` fgd) . map toDouble . opponentStealsByGame teams) teamObj
        , _tsd_blockPercentage                = average $ removeHomeAdvantage (const $ bd `divideDoubles` (bd `addDoubles` oppfg)) teamObj
        , _tsd_blockedPercentage              = average $ removeHomeAdvantage ((`divideDoubles` fgd) . map toDouble . opponentBlocksByGame teams) teamObj
        , _tsd_foulPercentage                 = average $ removeHomeAdvantage ((`divideDoubles` oppfg) . map toDouble . foulsByGame) teamObj
        , _tsd_fouledPercentage               = average $ removeHomeAdvantage ((`divideDoubles` fgd) . map toDouble . opponentFoulsByGame teams) teamObj
        , _tsd_playerProbabilities           = pps
        }