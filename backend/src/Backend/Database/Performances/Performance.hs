{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Backend.Database.Performances.Performance where
 
import Control.Lens
import Data.Int      (Int32)
import Data.Text     (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

data PerformanceT f = PerformanceT
  { _performance_t_playerName             :: Columnar f Text
  , _performance_t_matchupId              :: Columnar f Text
  , _performance_t_status                 :: Columnar f Text
  , _performance_t_points                 :: Columnar f Integer
  , _performance_t_offensiveRebounds      :: Columnar f Integer
  , _performance_t_defensiveRebounds      :: Columnar f Integer
  , _performance_t_assists                :: Columnar f Integer
  , _performance_t_steals                 :: Columnar f Integer
  , _performance_t_blocks                 :: Columnar f Integer
  , _performance_t_turnovers              :: Columnar f Integer
  , _performance_t_fieldGoalsMade         :: Columnar f Integer
  , _performance_t_fieldGoalsAttempted    :: Columnar f Integer
  , _performance_t_freeThrowsMade         :: Columnar f Integer
  , _performance_t_freeThrowsAttempted    :: Columnar f Integer
  , _performance_t_threePointersMade      :: Columnar f Integer
  , _performance_t_threePointersAttempted :: Columnar f Integer
  , _performance_t_personalFouls          :: Columnar f Integer
  , _performance_t_minutes                :: Columnar f Text
  , _performance_t_fic                    :: Columnar f Text
  }
makeLenses ''PerformanceT

deriving instance Generic (PerformanceT f)
deriving instance Beamable PerformanceT

type PerformanceRow = PerformanceT Identity

deriving instance Show PerformanceRow
deriving instance Eq PerformanceRow
deriving instance Ord PerformanceRow

instance Table PerformanceT where
  data PrimaryKey PerformanceT f = PerformanceId
    { performance_id_playerName :: Columnar f Text
    , performance_id_matchupId  :: Columnar f Text
    }
  primaryKey PerformanceT{..} = PerformanceId _performance_t_playerName _performance_t_matchupId

deriving instance Generic (PrimaryKey PerformanceT f)
deriving instance Beamable (PrimaryKey PerformanceT)

type PerformanceId = PrimaryKey PerformanceT Identity

deriving instance Show PerformanceId
deriving instance Eq PerformanceId
deriving instance Ord PerformanceId