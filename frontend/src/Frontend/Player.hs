{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Frontend.Player where

import Reflex.Dom.Core hiding (Element)

import           Control.Arrow          ((***))
import           Control.Lens
import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Data.Foldable          (for_)
import           Data.Functor           (void)
import qualified Data.Map.Ordered       as O
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           JSDOM.Generated.Document (createElement)
-- import           GHCJS.DOM.Element      (setInnerHTML)
-- import           GHCJS.DOM.Types        (liftJSM)
import           Obelisk.Route.Frontend
 
import           Common.Api.Matchups.Matchup
import           Common.Api.Performances.Performance
import           Common.Api.Players.Player
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Api.Namespace              (unNamespace)
import           Common.Probability                (average)
import           Common.Route
import           Frontend.Chart
import qualified Frontend.Client                   as Client
-- import           Frontend.FrontendStateT
import           Frontend.Utils

playerPage
  :: forall t m
  .  ( DomBuilder t m
     , Prerender t m
     , Routed t (TeamName, PlayerName) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => m ()
playerPage = do
  -- We ask our route for the document slug and make the backend call on load
  playerNameDyn <- askRoute
  pbE <- getPostBuild
  let brDyn = (\s -> BackendRoute_Api :/ ApiRoute_Player :/ s) <$> playerNameDyn
  successE <- Client.backendGET brDyn
  widgetHold blank $ maybe blank (playerContent playerNameDyn) <$> successE
  pure ()

playerContent
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Dynamic t (TeamName, PlayerName)
  -> (Player, [Performance])
  -> m ()
playerContent playerNameDyn (player', performances) = elClass "div" "player-page" $ do
  elClass "div" "player-title" $ el "h1" $ dynText $ unPlayerName . snd <$> playerNameDyn
  playerTable player' performances
  pure ()

playerTable
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Player
  -> [Performance]
  -> m ()
playerTable player performances = do
  el "div" $ do
    el "div" $ el "h2" $ text "Status"
    el "div" $ do
      el "div" $ el "h3" $ text "Histogram"
      el "div" $ histogramChart _performance_status performances

  chartsIntegerWidget "Points" performances _performance_points
  chartsIntegerWidget "Offensive Rebounds" performances _performance_offensiveRebounds
  chartsIntegerWidget "Defensive Rebounds" performances _performance_defensiveRebounds
  chartsIntegerWidget "Assists" performances _performance_assists
  chartsIntegerWidget "Steals" performances _performance_steals
  chartsIntegerWidget "Blocks" performances _performance_blocks
  chartsIntegerWidget "Turnovers" performances _performance_turnovers
  chartsIntegerWidget "Field Goals Made" performances _performance_fieldGoalsMade
  chartsIntegerWidget "Field Goals Attempted" performances _performance_fieldGoalsAttempted
  chartsIntegerWidget "Free Throws Made" performances _performance_freeThrowsMade
  chartsIntegerWidget "Free Throws Attempted" performances _performance_freeThrowsAttempted
  chartsIntegerWidget "Three Pointers Made" performances _performance_threePointersMade
  chartsIntegerWidget "Three Pointers Attempted" performances _performance_threePointersAttempted
  chartsIntegerWidget "Personal Fouls" performances _performance_personalFouls
  pure ()

playerImage
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Text -- Class
  -> Dynamic t Text
  -> m ()
playerImage className imageDyn =
  elDynAttr "img"
    ((\i -> M.fromList [("src",imgUrl $ Just i),("class",className)]) <$> imageDyn)
    blank
