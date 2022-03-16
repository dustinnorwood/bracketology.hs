{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Api.Teams
  ( TeamsApi
  , TeamApi
  , Windowed(..)
  , limit
  , offset
  , item
  , GetTeams
  , module Team
  , module Namespace
  ) where

import Servant.API

import Control.Lens
import Data.Text    (Text)

import Common.Api.Teams.Team as Team
import Common.Api.Namespace  as Namespace (Namespace (Namespace))

data Windowed a = Windowed
  { _limit       :: Maybe Integer
  , _offset      :: Maybe Integer
  , _item        :: a
  } deriving (Eq, Ord, Show)
makeLenses ''Windowed

type GetTeams     = Windowed Text

type TeamsApi =
  (
     QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> Get '[JSON] [Team]
  ) :<|> TeamApi


type TeamApi = (
  Capture "name" Text
  :> (
    Get '[JSON] (Namespace "team" Team)
    )
  )
