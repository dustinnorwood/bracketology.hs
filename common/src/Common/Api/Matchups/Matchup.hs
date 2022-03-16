{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Api.Matchups.Matchup where

import Control.Lens
import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Bool
import Data.Int                    (Int32)
import qualified Data.Map.Ordered  as O
import Data.Map.Strict             (Map)
import qualified Data.Map.Strict   as M
import Data.Text                   (Text)
import qualified Data.Text         as T
import GHC.Generics                (Generic)
import Common.Api.Teams.Team
import Common.Api.Performances.Performance

data Matchup = Matchup
  { _matchup_teamName     :: Text
  , _matchup_opponent     :: Text
  , _matchup_win          :: Bool
  , _matchup_home         :: Bool
  , _matchup_teamScore    :: Integer
  , _matchup_oppScore     :: Integer
  , _matchup_id           :: Text
  } deriving (Eq, Show)

makeLenses ''Matchup

deriving instance Generic Matchup
deriving instance ToJSON Matchup
deriving instance FromJSON Matchup

team_wins :: [Matchup] -> Int
team_wins = foldr (\a b -> b + (bool 0 1 $ _matchup_win a)) 0

team_losses :: [Matchup] -> Int
team_losses = foldr (\a b -> b + (bool 1 0 $ _matchup_win a)) 0

team_gamesPlayed :: [Matchup] -> Int
team_gamesPlayed = length

team_winPercentage :: [Matchup] -> Double
team_winPercentage ms =
  let ~(w,x) = foldr (\a (v,u) -> (v + (bool 1 0 $ _matchup_win a), u+1)) (1, 2) ms
   in fromInteger w / fromInteger x

getMatchupId :: Matchup -> Text
getMatchupId Matchup{..} =
  let oScore = T.pack $ show _matchup_oppScore
      tScore = T.pack $ show _matchup_teamScore
   in _matchup_opponent <> tScore <> oScore

getOppMatchupId :: Text -> Matchup -> Text
getOppMatchupId teamName Matchup{..} =
  let oScore = T.pack $ show _matchup_oppScore
      tScore = T.pack $ show _matchup_teamScore
   in teamName <> oScore <> tScore