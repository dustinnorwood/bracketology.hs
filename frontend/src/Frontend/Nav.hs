{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, TypeFamilies, TupleSections #-}
module Frontend.Nav where

import Reflex.Dom.Core

import Control.Lens
import           Control.Monad.Fix      (MonadFix)
import Control.Monad.IO.Class
import Data.Bool              (bool)
import Data.Functor           (void)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text              (Text)
import qualified Data.Text    as T
import Data.Traversable       (for)
import Obelisk.Route          (pattern (:/), R)
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute, askRoute, setRoute)

import           Common.Api.Teams
import           Common.Route
import           Frontend.Client                 (urlGET, backendGET)
import           Frontend.TeamPreview
import           Frontend.Utils                  (routeLinkDynClass)

nav
  :: ( DomBuilder t m
     , PerformEvent t m
     , Prerender t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => m ()
nav = do
  rDyn <- askRoute
  el "header" . elClass "nav" "nav-container navbar navbar-expand-md navbar-light fixed-top bg-light" $ do
    divClass "nav-left" $ do
      routeLinkDynClass "navbar-brand" (constDyn $ homeRoute) $ text "Bracketology"
      searchWidget
    divClass "nav-right" $ do
      elAttr "button" ("class"=:"navbar-toggler"
                      <> "type"=:"button"
                      <> "data-toggle"=:"collapse"
                      <> "data-target"=:"#navbarCollapse"
                      <> "aria-controls"=:"navbarCollapse"
                      <> "aria-expanded"=:"false"
                      <> "aria-label"=:"Toggle navigation"
                      ) $
          elClass "span" "navbar-toggler-icon" blank
      elAttr "div" ("class" =: "collapse navbar-collapse" <> "id"=:"navbarCollapse") $ do
        elClass "ul" "navbar-nav mr-auto" $ do
          navItem homeRoute rDyn $ text "Home"
  
  where
    navItem r rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== r) <$> rDyn)
      (constDyn r)

searchWidget
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , PerformEvent t m
     , PostBuild t m
     , Prerender t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => m ()
searchWidget = do
  divClass "dropdown" $ mdo
    ti <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Search term..." <> "style" =: "border-radius: 0;")
      & inputElementConfig_setValue .~ clearEv
    let srch = ffor (value ti) $ \t -> rdef & item . search .~ t
                                            & limit ?~ 5
        isEmpty t | t ^. item . search == "" = Nothing
                  | otherwise                = Just t
    srchEv <- debounce 0.5 . fmapMaybe isEmpty $ updated srch
    srchDyn <- holdDyn emptySearchTeams srchEv
    searchResultsE <- backendGET $ (\term -> BackendRoute_Api :/ ApiRoute_Teams :/ TeamsRoute_Search :/ term) <$> srchDyn
    searchResults <- holdDyn M.empty (fromMaybe M.empty <$> searchResultsE)
    let noResults = M.null <$> searchResults
        dNone = bool M.empty ("style" =: "display: none;") <$> noResults
        searchAttrs = ("class" =: "dropdown-content" <>) <$> dNone
    clickedDyn <- elDynAttr "div" searchAttrs $ listWithKey searchResults searchDropDownItem
    let enterEv = tag (current $ value ti) $ keypress Enter ti
        elemsDyn = M.elems <$> clickedDyn
        clickedItem = switchDyn $ leftmost . map snd <$> elemsDyn
        clearEv = leftmost ["" <$ enterEv, "" <$ clickedItem]
        searchPageQuery = ffor enterEv $ \t -> rdef & item . search .~ t
                                                    & limit ?~ 25
    setRoute ((\t -> FrontendRoute_Team :/ (TeamName t)) <$> clickedItem)
    setRoute ((\t -> FrontendRoute_Search :/ t) <$> searchPageQuery)
    return ()
  where searchDropDownItem slug pkgModelDyn = do
          (r,_) <- el' "div" $ teamPreviewSmall $ (slug,) <$> pkgModelDyn
          pure (slug, slug <$ domEvent Click r)