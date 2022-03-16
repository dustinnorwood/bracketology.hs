{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Api
  ( module Common.Api
  , module Namespace
  , module Teams
  -- , module Players
  -- , module Matchups
  -- , module Performances
  ) where

import Data.Proxy  (Proxy (..))
import Servant.API ((:>))

import Common.Api.Namespace    as Namespace
import Common.Api.Teams        as Teams
-- import Common.Api.Players      as Players
-- import Common.Api.Matchups     as Matchups
-- import Common.Api.Performances as Performances

type Api = "api" :> TopLevelApi

type TopLevelApi
     =    ("teams"         :> TeamsApi)
     -- :<|> ("players"       :> PlayersApi)
     -- :<|> ("matchups"      :> MatchupsApi)
     -- :<|> ("performances"  :> PerformancesApi)

api :: Proxy Api
api = Proxy