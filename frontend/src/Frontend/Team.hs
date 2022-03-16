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

module Frontend.Team where

import Reflex.Dom.Core hiding (Element)

import           Control.Arrow          ((***))
import           Control.Lens
import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Data.Bifunctor         (first)
import           Data.Foldable          (for_)
import           Data.Function          (on)
import           Data.Functor           (void)
import           Data.List              (sortBy)
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

teamPage
  :: forall t m
  .  ( DomBuilder t m
     , Prerender t m
     , Routed t TeamName m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => m ()
teamPage = do
  -- We ask our route for the document slug and make the backend call on load
  el "br" blank
  el "br" blank
  el "br" blank
  el "br" blank
  el "br" blank
  teamNameDyn <- askRoute
  pbE <- getPostBuild
  let brDyn = (\s -> BackendRoute_Api :/ ApiRoute_Team :/ TeamRoute_Get :/ s) <$> teamNameDyn
  successE <- Client.backendGET brDyn
  widgetHold blank $ maybe blank (teamContent teamNameDyn) <$> successE
  pure ()

teamContent
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Dynamic t TeamName
  -> TeamObject
  -> m ()
teamContent teamNameDyn teamObj = elClass "div" "team-page" $ do
  elClass "div" "team-title" $ el "h1" $ text $ teamObj ^. team . team_name
  teamTable teamObj
  el "div" $ matchupTable teamObj
  el "div" $ playerTable teamObj
  pure ()

teamTable
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => TeamObject
  -> m ()
teamTable teamObj = el "div" $ do
  let matches = map snd . O.assocs $ teamObj ^. matchups
      scores = (\m -> (_matchup_teamScore m, fromInteger $ _matchup_oppScore m)) <$> matches
      sortedScores = sortBy (compare `on` fst) scores
      textScores = (first $ T.pack . show) <$> sortedScores
  el "div" $ scatterPlotWidget textScores
  chartsIntegerWidget "Points Scored" matches _matchup_teamScore
  chartsIntegerWidget "Points Allowed" matches _matchup_oppScore

matchupTable
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => TeamObject
  -> m ()
matchupTable teamObj = do
  let matchups' = teamObj ^. matchups
      matchupsDyn = constDyn $ M.fromList . zip [0..] $ O.toAscList matchups'
      tableFuncs =
        [ ("Opponent", \_ m -> teamLink $ _matchup_opponent . snd <$> m)
        , ("Win",  \_ m -> dynText $ T.pack . show . _matchup_win . snd <$> m)
        , ("Team Score",  \_ m -> dynText $ T.pack . show . _matchup_teamScore . snd <$> m)
        , ("Opponent Score",  \_ m -> dynText $ T.pack . show . _matchup_oppScore . snd <$> m)
        , ("Home Game",  \_ m -> dynText $ T.pack . show . _matchup_home . snd <$> m)
        ]
      attrFunc _ = pure $ constDyn M.empty
  tableDynAttr "" tableFuncs matchupsDyn attrFunc
  pure ()

teamLink
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Dynamic t Text
  -> m ()
teamLink teamDyn = do
  (r,_) <- elAttr' "div" ("class" =: "item") $ dynText teamDyn
  let teamClickEv = tagPromptlyDyn teamDyn $ domEvent Click r
  setRoute ((\t -> FrontendRoute_Team :/ (TeamName t)) <$> teamClickEv)
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
  => TeamObject
  -> m ()
playerTable teamObj = do
  let playersDyn = constDyn $ teamObj ^. players
      pPerfs p = M.elems $ M.filterWithKey (\(n,_) _ -> p == n) (teamObj ^. performances)
      ppg = average . map (fromInteger . (^. performance_points))
      gp = length
      teamName = teamObj ^. team . team_name
      tableFuncs =
        [ ("Name", \_ m -> playerLink teamName $ _player_name <$> m)
        , ("Number", \_ m -> dynText $ T.pack . show . _player_number <$> m)
        , ("Position",  \_ m -> dynText $ _player_position <$> m)
        , ("Games Played", \k _ -> dynText . constDyn $ T.pack . show . gp $ pPerfs k)
        , ("Points per game", \k _ -> dynText . constDyn $ T.pack . show . ppg $ pPerfs k)
        ]
      attrFunc _ = pure $ constDyn M.empty
  tableDynAttr "" tableFuncs playersDyn attrFunc
  pure ()

playerLink
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Text
  -> Dynamic t Text
  -> m ()
playerLink teamName playerDyn = do
  (r,_) <- elAttr' "div" ("class" =: "item") $ dynText playerDyn
  let playerClickEv = tagPromptlyDyn playerDyn $ domEvent Click r
  setRoute ((\t -> FrontendRoute_Player :/ (TeamName teamName, PlayerName t)) <$> playerClickEv)
  pure ()

teamImage
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Text -- Class
  -> Dynamic t Text
  -> m ()
teamImage className imageDyn =
  elDynAttr "img"
    ((\i -> M.fromList [("src",imgUrl $ Just i),("class",className)]) <$> imageDyn)
    blank