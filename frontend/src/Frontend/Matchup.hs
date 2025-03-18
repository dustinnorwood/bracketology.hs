{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Common.Statistics.Offensive
import           Control.Applicative    (liftA2)
import           Control.Lens
import           Control.Monad          (join, replicateM, when)
import           Control.Monad.FT.Put
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.Bifunctor         (first)
import           Data.Bool              (bool)
import           Data.Foldable          (for_, foldl')
import           Data.Function          (on)
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
  ( MonadIO m
  ) => Puttable LogMessage (LoggingT m) where
  put (LogMessage msg) = LoggingT $ liftIO . putStrLn $ T.unpack msg 

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
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
bracketPage = do
  -- We ask our route for the document slug and make the backend call on load
  let bRoute = constDyn $ BackendRoute_Api :/ ApiRoute_Bracket :/ ()
  teamNames <- Client.backendGET bRoute
  widgetHold blank $ maybe blank bracketContent <$> teamNames
  pure ()

matchupPage
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , Routed t (TeamName, TeamName) m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
matchupPage = do
  matchDyn <- askRoute
  let bRoute = (\(TeamName t1, TeamName t2) -> BackendRoute_Api :/ ApiRoute_Matchup :/ MatchupRoute_Run :/ [t1, t2]) <$> matchDyn
  tsds <- Client.backendGET bRoute
  t1d <- holdDyn Nothing $ fmap fst <$> tsds
  t2d <- holdDyn Nothing $ fmap snd <$> tsds
  dataDyn <- elClass "div" "region" $ do
    dataAndWinnerDyn <- elClass "div" "matches round-2" $ matchWidget False t1d t2d 
    teamWidget "home" (snd <$> dataAndWinnerDyn) $ constDyn (0.0,0.0)
    pure $ fst <$> dataAndWinnerDyn
  void . widgetHold blank $ updated dataDyn <&> \case
    [] -> blank
    sds -> do
      el "table" $ do
        el "thead" $ do
          el "td" $ text "Game #"
          el "td" $ text "Winner"
          el "td" $ dynText $ (<> " Score") . unTeamName . fst <$> matchDyn
          el "td" $ dynText $ (<> " Score") . unTeamName . snd <$> matchDyn
        for_ (zip [0..] sds) $ \(i, (startingData, endingData)) -> do
          el "tr" $ do
            el "td" . text . T.pack . show $ i + 1
            let t1s = endingData ^. sd_team1 . ts_team_score
                t2s = endingData ^. sd_team2 . ts_team_score
                winner = Just $ if t1s > t2s
                           then endingData ^. sd_team1 . ts_team
                           else endingData ^. sd_team2 . ts_team
            el "td" $ do
              elAttr "img" (teamImg winner) blank
              text $ " " <> teamName winner
            el "td" . text . T.pack $ show t1s
            el "td" . text . T.pack $ show t2s

bracketContent
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => [TeamSimulationDetails]
  -> m ()
bracketContent tsds = elClass "div" "team-page" $ do
  pBE <- getPostBuild
  r64 <- traverse (\tsd -> holdDyn Nothing (Just tsd <$ pBE)) tsds
  bracketWidget r64

mapM2 :: Monad m => (a -> a -> m b) -> [a] -> m [b]
mapM2 f (a1:a2:as) = f a1 a2 >>= \b -> (b:) <$> mapM2 f as
mapM2 _ _          = pure []

traverseM2 :: (Applicative f, Monad m) => (a -> a -> m (f b)) -> [a] -> m (f [b])
traverseM2 f (a1:a2:as) = f a1 a2 >>= \b -> liftA2 (:) b <$> traverseM2 f as
traverseM2 _ _          = pure $ pure []

bracketWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => [Dynamic t (Maybe TeamSimulationDetails)]
  -> m ()
bracketWidget r68 = elClass "div" "region" $ do
  let playins = take 8 r68
      aqs = drop 8 r68
  first4 <- elClass "div" "matches round-first4" $ mapM2 bracketMatchWidget playins
  let r64 = take 1 aqs
         ++ take 1 first4
         ++ take 7 (drop 1 aqs)
         ++ take 1 (drop 1 first4)
         ++ take 23 (drop 8 aqs)
         ++ take 1 (drop 2 first4)
         ++ take 23 (drop 31 aqs)
         ++ drop 3 first4
         ++ drop 54 aqs
  r32 <- elClass "div" "matches round-64" $ mapM2 bracketMatchWidget r64
  s16 <- elClass "div" "matches round-32" $ mapM2 bracketMatchWidget r32
  e8 <- elClass "div" "matches round-16" $ mapM2 bracketMatchWidget s16
  f4 <- elClass "div" "matches round-8" $ mapM2 bracketMatchWidget e8
  n2 <- elClass "div" "matches round-4" $ mapM2 bracketMatchWidget f4
  c1 <- elClass "div" "matches round-2" $ mapM2 bracketMatchWidget n2
  let champion = case c1 of
        [] -> constDyn Nothing
        (x:_) -> x
  teamWidget "home" champion $ constDyn (0.0,0.0)

bracketMatchWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t (Maybe TeamSimulationDetails)
  -> Dynamic t (Maybe TeamSimulationDetails)
  -> m (Dynamic t (Maybe TeamSimulationDetails))
bracketMatchWidget t1 t2 = fmap snd <$> matchWidget True t1 t2

matchWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => Bool
  -> Dynamic t (Maybe TeamSimulationDetails)
  -> Dynamic t (Maybe TeamSimulationDetails)
  -> m (Dynamic t ([(SimulationData, SimulationData)], Maybe TeamSimulationDetails))
matchWidget clickable t1d t2d = do
  (e, w) <- elAttr' "div" ("class" =: "match") $ mdo
    teamWidget "home" t1d t12w
    teamWidget "visitor" t2d t21w
    let t12d = liftA2 (liftA2 (,)) t1d t2d
    winner' <- simulationWidget t12d
    let t12w = (\(_,w1,w2,_) -> (w1,w2)) <$> winner'
    let t21w = (\(w1,w2) -> (w2,w1)) <$> t12w
    winner <- holdUniqDynBy ((==) `on` (teamName . snd)) $ (\(s,_,_,t) -> (s,t)) <$> winner'
    pure winner
  when clickable $ do
    let teamsDyn = ((,) `on` (TeamName . teamName)) <$> t1d <*> t2d
    setRoute $ (FrontendRoute_Matchup :/) <$> tag (current teamsDyn) (domEvent Click e)
  pure w

teamWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => Text
  -> Dynamic t (Maybe TeamSimulationDetails)
  -> Dynamic t (Double, Double)
  -> m ()
teamWidget teamType teamDyn winsDyn = do
  elClass "div" ("team " <> teamType) $ do
    (e, _) <- el' "div" $ do
      elDynAttr "img" (teamImg <$> teamDyn) blank
      dynText $ (" " <>) . teamName <$> teamDyn
      elDynAttr "span" ((\(w1,w2) -> "style" =: ("color: " <> (case w1 `compare` w2 of LT -> "red"; EQ -> "black"; GT -> "green") <> ";")) <$> winsDyn)
        . dynText $ (\(w1,w2) -> if w1 == 0.0 && w2 == 0.0 then "" else " " <> T.pack (show $ fromIntegral (round $ 10000 * w1) / 100) <> "%") <$> winsDyn
    setRoute ((\t -> FrontendRoute_Team :/ (TeamName t)) <$> tag (teamName <$> current teamDyn) (domEvent Click e))

teamName :: Maybe TeamSimulationDetails -> Text
teamName Nothing = "TBD"
teamName (Just t) = t ^. tsd_teamObj . team . team_name

teamImg :: Maybe TeamSimulationDetails -> M.Map Text Text
teamImg Nothing = M.empty
teamImg (Just t) = "src" =: (t ^. tsd_teamObj . team . team_image) <> "style" =: "height: 16px;"

simulationWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Dynamic t (Maybe (TeamSimulationDetails, TeamSimulationDetails))
  -> m (Dynamic t ([(SimulationData, SimulationData)], Double, Double, Maybe TeamSimulationDetails))
simulationWidget t12d = fmap join . prerender (constDyn ([],0.0,0.0,Nothing) <$ blank) $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
  -- dynText . fmap (T.pack . show) =<< foldDyn (\_ n -> n + 1) 0 (updated t12d)
  simDatas <- runSimulation t12d
  simDataDyn <- foldDyn (\(_,_,sds) b -> sds ++ b) [] simDatas
  let sims = (\(a,b,c) -> (a,b, simDataToSim . snd <$> c)) <$> simDatas
  -- let tname t = t ^. tsd_teamObj . team . team_name
  diffs <- foldDyn buildDiffs M.empty $ (\(_,_,s) -> s) <$> sims
  winners <- foldDyn pickWinner (("", 0), ("", 0)) sims
  -- let chartData = updated $ (\ws -> [fmap toDouble $ fst ws, fmap toDouble $ snd ws]) <$> winners
  -- delayedRender $ basicHorizontalTimeBarChart "" chartData
  -- delayedRender $ histogramIntTimeChart $ updated diffs
  winner <- foldDyn predictWinner (0, 0, 0.0, 0.0, Nothing) sims
  -- dynText $ winnerText <$> winner
  pure $ (\(sds,(_,_,c,d,e)) -> (sds,c,d,e)) <$> liftA2 (,) simDataDyn winner
  where
    delayedRender m = do
      c <- wrapper m
      (ev1, _) <- headTailE (_chart_finished c)
      pure ()
    wrapper m = elAttr "div" ("style" =: "padding: 0px;") m
    -- title = t1 <> " vs. " <> t2
    teamName t = t ^. tsd_teamObj . team . team_name
    diff (Sim s1 s2) = toInteger $ round s2 - round s1
    buildDiffs = flip . foldl' $ \m sd -> (at (diff sd) %~ Just . maybe 1 (+1)) m
    pickWinner :: (TeamSimulationDetails, TeamSimulationDetails, [Sim Double]) -> ((Text, Integer), (Text, Integer)) -> ((Text, Integer), (Text, Integer))
    pickWinner (t1, t2, sds) = flip (foldl' (\m sd ->
      let m' = if diff sd > 0
                 then (_2 . _2 %~ (+1)) m
                 else (_1 . _2 %~ (+1)) m
       in m' & _1 . _1 .~ teamName t1
             & _2 . _1 .~ teamName t2)) sds
    predictWinner (_, _, []) b = b
    predictWinner (t1, t2, sds) (w, t, w1p, w2p, _) =
      let w' = w + (sum $ bool 1 0 . (>0) . diff <$> sds)
          len = t + (toInteger $ length sds)
          wins1 = fromIntegral w' / fromIntegral len
       in (w', len, wins1, 1.0 - wins1, case () of
            _ | w' > len - w' -> Just t1
            otherwise -> Just t2)
    winnerText Nothing = "Toss up"
    winnerText (Just t) = "Predicting " <> teamName t <> " to win"

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
  => Dynamic t (Maybe (TeamSimulationDetails, TeamSimulationDetails))
  -> m (Event t (TeamSimulationDetails, TeamSimulationDetails, [(SimulationData, SimulationData)]))
runSimulation t12d = fmap switchDyn . widgetHold (pure never) $ updated t12d <&> \case
  Nothing -> pure never
  Just (team1, team2) -> do
    -- tick <- tickSource
    tick <- getPostBuild
    ev <- performEvent $ ffor tick $ \_ -> replicateM 101 $ do
      seed <- liftIO $ getStdRandom random
      runLoggingT $ playGame team1 team2 seed
    return $ (team1,team2,) <$> ev

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
  dyn ((\v -> tickLossyFromPostBuildTime (fromRational $ toRational v)) <$> (constDyn 2.0))
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