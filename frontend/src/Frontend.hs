{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications                                #-}

module Frontend where

import Reflex.Dom.Core hiding (Namespace)

import Data.List.NonEmpty         (NonEmpty)
import Obelisk.Frontend           (Frontend (Frontend), ObeliskWidget)
import Obelisk.Route.Frontend     (R, RouteToUrl, RoutedT, SetRoute, subRoute_)

import Common.Route

import           Common.Route                    (FrontendRoute (..))
import           Frontend.Head                   (htmlHead)
-- import           Frontend.HomePage               (homePage)
--import           Frontend.Login                  (login)
import           Frontend.Matchup                (bracketPage, matchupPage)
import           Frontend.Nav                    (nav)
import           Frontend.Player                 (playerPage)
import           Frontend.Team                   (teamPage)
--import           Frontend.Profile                (profile)
--import           Frontend.Register               (register)
import           Frontend.Search                 (searchPage)
--import           Frontend.Settings               (settings)
import           Frontend.Utils                  (routeLinkClass)

htmlBody
  :: forall t m
  . ( ObeliskWidget t (R FrontendRoute) m
    )
  => RoutedT t (R FrontendRoute) m ()
htmlBody = do
  elClass "div" "grid-container" $ do
    elClass "div" "grid-item-nav" nav
    elClass "div" "grid-item-main" $ subRoute_ pages
    -- elClass "div" "grid-item-footer" footer
  where
    pages 
      :: FrontendRoute a
      -> RoutedT t a m ()
    pages r = case r of
      FrontendRoute_Home     -> bracketPage
      FrontendRoute_Search   -> searchPage
      --FrontendRoute_Login    -> login
      --FrontendRoute_Register -> register
      FrontendRoute_Team -> teamPage
      FrontendRoute_Player -> playerPage
      FrontendRoute_Matchup -> matchupPage
      --FrontendRoute_Settings -> settings
      --FrontendRoute_Profile  -> pathSegmentSubRoute profile

footer
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R (FrontendRoute)) m
     , SetRoute t (R (FrontendRoute)) m
     , MonadSample t m
     )
  => m ()
footer = elClass "footer" "footer" $ elClass "div" "container" $ do
  elClass "div" "brand" $ routeLinkClass "logo-font" homeRoute $ text "Bracketology"
  elClass "span" "attribution" $ do
    text "The trillion dollar business opportunity."

frontend :: Frontend (R FrontendRoute)
frontend = Frontend (prerender_ htmlHead htmlHead) htmlBody