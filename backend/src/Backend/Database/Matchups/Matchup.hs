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

module Backend.Database.Matchups.Matchup where

import Control.Lens
import Data.Int      (Int32)
import Data.Text     (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

data MatchupT f = MatchupT
  { _matchup_t_id        :: Columnar f Text
  , _matchup_t_teamName  :: Columnar f Text
  , _matchup_t_opponent  :: Columnar f Text
  , _matchup_t_win       :: Columnar f Bool
  , _matchup_t_home      :: Columnar f Bool
  , _matchup_t_teamScore :: Columnar f Integer
  , _matchup_t_oppScore  :: Columnar f Integer
  }
makeLenses ''MatchupT

deriving instance Generic (MatchupT f)
deriving instance Beamable MatchupT

type MatchupRow = MatchupT Identity

deriving instance Show MatchupRow
deriving instance Eq MatchupRow
deriving instance Ord MatchupRow

instance Table MatchupT where
  data PrimaryKey MatchupT f = MatchupId
    { unMatchupId :: Columnar f Text
    }
  primaryKey = MatchupId . _matchup_t_id

deriving instance Generic (PrimaryKey MatchupT f)
deriving instance Beamable (PrimaryKey MatchupT)

type MatchupId = PrimaryKey MatchupT Identity

deriving instance Show MatchupId
deriving instance Eq MatchupId
deriving instance Ord MatchupId
