{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Api.Teams.Team where

import Control.Lens
import Data.Aeson
import Data.Text                   (Text)
import GHC.Generics                (Generic)

data Team = Team
  { _team_url   :: Text
  , _team_name  :: Text
  , _team_image :: Text
  , _team_score :: Double
  } deriving (Eq, Show)

makeLenses ''Team

deriving instance Generic Team
deriving instance ToJSON Team
deriving instance FromJSON Team