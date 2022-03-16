{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Api.Performances.Performance where

import Control.Lens
import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Function               (on)
import Data.Int                    (Int32)
import Data.List                   (sortBy)
import Data.Maybe
import Data.Text                   (Text)
import qualified Data.Text         as T
import GHC.Generics                (Generic)
import Common.Api.Players.Player
import Text.Read

data Performance = Performance
  { _performance_player                 :: Text
  , _performance_matchupId              :: Text
  , _performance_status                 :: Text
  , _performance_points                 :: Integer
  , _performance_offensiveRebounds      :: Integer
  , _performance_defensiveRebounds      :: Integer
  , _performance_assists                :: Integer
  , _performance_steals                 :: Integer
  , _performance_blocks                 :: Integer
  , _performance_turnovers              :: Integer
  , _performance_fieldGoalsMade         :: Integer
  , _performance_fieldGoalsAttempted    :: Integer
  , _performance_freeThrowsMade         :: Integer
  , _performance_freeThrowsAttempted    :: Integer
  , _performance_threePointersMade      :: Integer
  , _performance_threePointersAttempted :: Integer
  , _performance_personalFouls          :: Integer
  , _performance_minutes                :: Text
  , _performance_fic                    :: Text
  } deriving (Eq, Show)

makeLenses ''Performance

deriving instance Generic Performance
deriving instance ToJSON Performance
deriving instance FromJSON Performance

getStarters :: [(Player, [Performance])] -> [Player]
getStarters = take 5 . sortPlayersByMinutes

sortPlayersByMinutes :: [(Player, [Performance])] -> [Player]
sortPlayersByMinutes = map fst . sortBy (compare `on` sumMinutes)
  where sumMinutes = sum . map (fromMaybe (0.0 :: Double) . readMaybe . T.unpack . _performance_minutes) . snd