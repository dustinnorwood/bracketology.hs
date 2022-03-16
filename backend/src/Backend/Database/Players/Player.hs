{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Backend.Database.Players.Player where
 
import Control.Lens
import Data.Int      (Int32)
import Data.Text     (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

data PlayerT f = PlayerT
  { _player_t_name        :: Columnar f Text
  , _player_t_teamName    :: Columnar f Text
  , _player_t_position    :: Columnar f Text
  , _player_t_gamesPlayed :: Columnar f Integer
  , _player_t_height      :: Columnar f Integer
  , _player_t_weight      :: Columnar f Integer
  , _player_t_number      :: Columnar f Integer
  , _player_t_class       :: Columnar f Text
  , _player_t_birthDate   :: Columnar f Text
  , _player_t_birthCity   :: Columnar f Text
  , _player_t_nationality :: Columnar f Text
  , _player_t_highSchool  :: Columnar f Text
  }
makeLenses ''PlayerT

deriving instance Generic (PlayerT f)
deriving instance Beamable PlayerT

type PlayerRow = PlayerT Identity

deriving instance Show PlayerRow
deriving instance Eq PlayerRow
deriving instance Ord PlayerRow

instance Table PlayerT where
  data PrimaryKey PlayerT f = PlayerId
    { unPlayerId :: Columnar f Text
    }
  primaryKey = PlayerId . _player_t_name

deriving instance Generic (PrimaryKey PlayerT f)
deriving instance Beamable (PrimaryKey PlayerT)

type PlayerId = PrimaryKey PlayerT Identity

deriving instance Show PlayerId
deriving instance Eq PlayerId
deriving instance Ord PlayerId