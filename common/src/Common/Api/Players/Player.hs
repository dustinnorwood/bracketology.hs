{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Api.Players.Player where

import Control.Lens
import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Int                    (Int32)
import Data.Text                   (Text)
import GHC.Generics                (Generic)

data Player = Player
  { _player_name         :: Text
  , _player_team_name    :: Text
  , _player_position     :: Text
  , _player_gamesPlayed  :: Integer
  , _player_height       :: Integer
  , _player_weight       :: Integer
  , _player_number       :: Integer
  , _player_class        :: Text
  , _player_birthDate    :: Text
  , _player_birthCity    :: Text
  , _player_nationality  :: Text
  , _player_highSchool   :: Text
  } deriving (Eq, Show)

makeLenses ''Player

deriving instance Generic Player
deriving instance ToJSON Player
deriving instance FromJSON Player
