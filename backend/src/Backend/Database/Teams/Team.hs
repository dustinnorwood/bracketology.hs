{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Backend.Database.Teams.Team where

import Prelude hiding (id)
 
import Control.Lens
import Data.Aeson    (FromJSON (..), ToJSON (..))
import Data.Int      (Int32)
import Data.Text     (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

data TeamT f = TeamT
  { _team_t_url         :: Columnar f Text
  , _team_t_name        :: Columnar f Text
  , _team_t_image       :: Columnar f Text
  , _team_t_score       :: Columnar f Double
  }

makeLenses ''TeamT

deriving instance Generic (TeamT f)
deriving instance Beamable TeamT

type TeamRow = TeamT Identity

deriving instance Show TeamRow
deriving instance Eq TeamRow
deriving instance Ord TeamRow
deriving instance ToJSON TeamRow
deriving instance FromJSON TeamRow

instance Table TeamT where
  data PrimaryKey TeamT f = TeamId
    { unTeamId :: Columnar f Text
    }
  primaryKey = TeamId . _team_t_name

deriving instance Generic (PrimaryKey TeamT f)
deriving instance Beamable (PrimaryKey TeamT)

type TeamId = PrimaryKey TeamT Identity

deriving instance Show TeamId
deriving instance Eq TeamId
deriving instance Ord TeamId