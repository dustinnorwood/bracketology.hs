{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Matchup where

import Reflex.Dom.Core hiding (Element)

import           Control.Lens
import           Control.Monad          (join, replicateM)
import           Control.Monad.FT.Put
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.Bifunctor         (first)
import           Data.Foldable          (for_, foldl')
import           Data.Functor           (void)
import qualified Data.Map.Ordered       as O
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           JSDOM.Generated.Document (createElement)
import           System.Random

import Language.Javascript.JSaddle hiding ((!!))
-- import           GHCJS.DOM.Element      (setInnerHTML)
-- import           GHCJS.DOM.Types        (liftJSM)
import           Obelisk.Route.Frontend
import           Text.Read                 (readMaybe)
 
import           Common.Api.Matchups.Matchup
import           Common.Api.Performances.Performance
import           Common.Api.Players.Player
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Api.Namespace              (unNamespace)
import           Common.Logging
import           Common.Probability                (average)
import           Common.Route
import           Common.Simulation
import           Common.Simulation.Details
import           Common.Statistics.Util
import           Frontend.Chart
import qualified Frontend.Client                   as Client
-- import           Frontend.FrontendStateT
import           Frontend.Utils
import           Reflex.Dom.Widget.ECharts

newtype LoggingT m a = LoggingT { runLoggingT :: m a }
  deriving (Functor)

instance Applicative m => Applicative (LoggingT m) where
  pure = LoggingT . pure
  (LoggingT f) <*> (LoggingT a) = LoggingT $ f <*> a

instance Monad m => Monad (LoggingT m) where
  return = pure
  (LoggingT ma) >>= amb = LoggingT $ ma >>= \a ->
    let (LoggingT mb) = amb a
     in mb

instance 
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Puttable LogMessage (LoggingT m) where
  put (LogMessage msg) = LoggingT $ do
    el "br" blank
    text msg

newtype NoLoggingT m a = NoLoggingT { runNoLoggingT :: m a }
  deriving (Functor)

instance Applicative m => Applicative (NoLoggingT m) where
  pure = NoLoggingT . pure
  (NoLoggingT f) <*> (NoLoggingT a) = NoLoggingT $ f <*> a

instance Monad m => Monad (NoLoggingT m) where
  return = pure
  (NoLoggingT ma) >>= amb = NoLoggingT $ ma >>= \a ->
    let (NoLoggingT mb) = amb a
     in mb

instance 
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Puttable LogMessage (NoLoggingT m) where
  put _ = pure ()

bracketPage
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender t m
     )
  => m ()
bracketPage = do
  -- We ask our route for the document slug and make the backend call on load
  let bRoute = constDyn $ BackendRoute_Api :/ ApiRoute_Bracket :/ ()
  teamNames <- Client.backendGET bRoute
  widgetHold blank $ maybe blank bracketContent <$> teamNames
  pure ()

bracketContent
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => [TeamSimulationDetails]
  -> m ()
bracketContent tsds = elClass "div" "team-page" $ do
  mapM2_ bracketWidget tsds
  pure ()

mapM2_ :: Monad m => (a -> a -> m ()) -> [a] -> m ()
mapM2_ f (a1:a2:as) = f a1 a2 >> mapM2_ f as
mapM2_ _ _          = pure ()

bracketWidget :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => TeamSimulationDetails
  -> TeamSimulationDetails
  -> m ()
bracketWidget t1 t2 = do
  let t1name = t1 ^. tsd_teamObj . team . team_name
  let t2name = t2 ^. tsd_teamObj . team . team_name
  el "div" $ text $ t1name <> " vs. " <> t2name
  simulationWidget t1 t2
  pure ()

simulationWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => TeamSimulationDetails
  -> TeamSimulationDetails
  -> m ()
simulationWidget team1 team2 = prerender_ blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
  sims <- runSimulation team1 team2
  let t1name = team1 ^. tsd_teamObj . team . team_name
      t2name = team2 ^. tsd_teamObj . team . team_name
  diffs <- foldDyn buildDiffs M.empty sims
  winners <- foldDyn pickWinner ((t1name, 0), (t2name, 0)) sims
  let chartData = updated $ (\ws -> [fmap toDouble $ fst ws, fmap toDouble $ snd ws]) <$> winners
  delayedRender $ basicHorizontalTimeBarChart title chartData
  delayedRender $ histogramIntTimeChart $ updated diffs
  pure ()
  where
    delayedRender m = do
      c <- wrapper m
      (ev1, _) <- headTailE (_chart_finished c)
      pure ()
    wrapper m = elAttr "div" ("style" =: "padding: 0px;") m
    title = t1 <> " vs. " <> t2
    t1 = team1 ^. tsd_teamObj . team . team_name 
    t2 = team2 ^. tsd_teamObj . team . team_name 
    diff (Sim s1 s2) = toInteger $ round s2 - round s1
    buildDiffs = flip . foldl' $ \m sd -> (at (diff sd) %~ Just . maybe 1 (+1)) m
    pickWinner :: [Sim Double] -> ((Text, Integer), (Text, Integer)) -> ((Text, Integer), (Text, Integer))
    pickWinner = flip . foldl' $ \m sd ->
      if diff sd > 0
        then (_2 . _2 %~ (+1)) m
        else (_1 . _2 %~ (+1)) m
    predictWinner _ _ [] = "Toss up"
    predictWinner t1name t2name sds@(sd:_) =
      let signumSum = sum $ signum . diff <$> sds
          len = toInteger $ length sds
       in case () of
            _ | signumSum > len `div` 3 -> "Predicting " <> t2name <> " to win"
            _ | negate signumSum > len `div` 3 -> "Predicting " <> t1name <> " to win"
            otherwise -> "Toss up"

runSimulation
  :: forall t m .
     ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , MonadFix m
     , MonadIO (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     )
  => TeamSimulationDetails
  -> TeamSimulationDetails
  -> m (Event t [Sim Double])
runSimulation team1 team2 = do
  tick <- tickSource
  rec
    ev <- performEvent $ ffor tick $ \_ -> replicateM 2 $ do
      seed <- liftIO $ getStdRandom random
      let s = runIdentity $ simulateGame team1 team2 seed
      return s
  return ev

tickSource
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => m (Event t TickInfo)
tickSource = do
  dyn ((\v -> tickLossyFromPostBuildTime (fromRational $ toRational v)) <$> (constDyn 0.2))
    >>= switchHold never


--- Not used at the moment: Used for custom matchup selection
-- teamList
--   :: ( DomBuilder t m
--      , PostBuild t m
--      , MonadFix m
--      , MonadHold t m
--      , MonadSample t m
--      )
--   => [Text]
--   -> m (Dropdown t Text)
-- teamList teamNames = do
--   let namesDyn = constDyn . M.fromList . map (\x -> (x,x)) $ teamNames
--       ddConfig = DropdownConfig
--         { _dropdownConfig_setValue = never
--         , _dropdownConfig_attributes = constDyn mempty
--         }
--   dropdown "" namesDyn ddConfig
-- 
-- matchupWidget
--   :: ( DomBuilder t m
--      , PostBuild t m
--      , MonadFix m
--      , MonadHold t m
--      , MonadSample t m
--      , Prerender t m
--      )
--   => Dropdown t Text
--   -> Dropdown t Text
--   -> m ()
-- matchupWidget team1Dropdown team2Dropdown = do
--   let dropdownsDyn = (,)
--               <$> team1Dropdown ^. dropdown_value
--               <*> team2Dropdown ^. dropdown_value
--   let buttonAttrs = dropdownsDyn <&> \v1v2 ->
--         if not (T.null $ fst v1v2) && not (T.null $ snd v1v2)
--           then M.empty --"disabled" =: "true"
--           else M.empty --"disabled" =: "false"
--   
--   (e, _) <- elDynAttr' "button" buttonAttrs $ text "Simulate!"
--   let clickEv = domEvent Click e
--       taggedClickEv = tagPromptlyDyn dropdownsDyn clickEv
--       bRoute = (\(t1, t2) -> BackendRoute_Api :/ ApiRoute_Matchup :/ MatchupRoute_Run :/ [t1, t2]) <$> taggedClickEv
--   (teamsEv :: Event t (Maybe (TeamSimulationDetails, TeamSimulationDetails))) <- Client.backendGETEv bRoute
--   widgetHold blank $ maybe blank simulationWidget <$> teamsEv
--   pure ()