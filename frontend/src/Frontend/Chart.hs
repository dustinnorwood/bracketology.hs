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

module Frontend.Chart where

import Reflex.Dom.Core hiding (Element)

import           Control.Arrow          ((***))
import           Control.Lens
import           Control.Monad          (join)
import           Control.Monad.FT.Put
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.Bifunctor         (first)
import           Data.Foldable          (for_)
import           Data.Functor           (void)
import qualified Data.Map.Ordered       as O
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           JSDOM.Generated.Document (createElement)

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
import qualified Frontend.Client                   as Client
-- import           Frontend.FrontendStateT
import           Frontend.Utils
import           Reflex.Dom.Widget.ECharts

chartsIntegerWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Text
  -> [a]
  -> (a -> Integer)
  -> m ()
chartsIntegerWidget name input f = el "div" $ do
  el "div" $ el "h2" $ text name
  elAttr "div" ("style" =: "display: flex") $ do
    el "div" $ do
      el "div" $ el "h3" $ text "By Game"
      el "div" $ verticalBarChartWidget $ (T.pack . show *** f) <$> zip [0..] input
    el "div" $ do
      el "div" $ el "h3" $ text "Histogram"
      el "div" $ histogramIntChart f input

histogramChart
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , Show a
     , Ord a
     )
  => (s -> a)
  -> [s]
  -> m ()
histogramChart _ [] = verticalBarChartWidget ([] :: [(a, Int)])
histogramChart f ss = do
  let histogramMap = foldr buildHistogram M.empty ss
      keys = M.keysSet histogramMap
      minBin = minimum keys
      maxBin = maximum keys
  verticalBarChartWidget $ (first $ T.pack . show) <$> M.toList histogramMap
  where buildHistogram s m =
          let bin = f s
              prevVal = fromMaybe (0 :: Int) $ M.lookup bin m
           in M.insert bin (prevVal + 1) m

histogramIntChart
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => (s -> Integer)
  -> [s]
  -> m ()
histogramIntChart _ [] = verticalBarChartWidget ([] :: [(Text, Integer)])
histogramIntChart f ss = do
  let histogramMap = foldr buildHistogram M.empty ss
      keys = M.keysSet histogramMap
      minBin = minimum keys
      maxBin = maximum keys
      histogram = (\b -> (b, fromMaybe (0 :: Integer) $ M.lookup b histogramMap)) <$> [minBin..maxBin]
  verticalBarChartWidget $ (first $ T.pack . show) <$> histogram
  where buildHistogram s m =
          let bin = f s
              prevVal = fromMaybe 0 $ M.lookup bin m
           in M.insert bin (prevVal + 1) m

histogramIntTimeChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     )
  => Event t (M.Map Integer Integer)
  -> m (Chart t)
histogramIntTimeChart histogramMapEv = do
  let histogram = histogramMapEv <&> \histogramMap ->
        let keys = M.keysSet histogramMap
            minBin = minimum keys
            maxBin = maximum keys
         in (\b -> (b, toDouble . fromMaybe 0 $ M.lookup b histogramMap)) <$> [minBin..maxBin]
  basicVerticalTimeBarChart "" $ map (first $ T.pack . show) <$> histogram

horizontalBarChartWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , ToDouble a
     )
  => [(Text, a)]
  -> m ()
horizontalBarChartWidget chartData = do
  prerender_ blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
    delayedRender $ basicHorizontalBarChart chartData
    pure ()
  where
    delayedRender m = do
      c <- wrapper m
      (ev1, _) <- headTailE (_chart_finished c)
      pure ()
    wrapper m = elAttr "div" ("style" =: "padding: 0px;") m

verticalBarChartWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     , ToDouble a
     )
  => [(Text, a)]
  -> m ()
verticalBarChartWidget chartData = do
  prerender_ blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
    delayedRender $ basicVerticalBarChart chartData
    pure ()
  where
    delayedRender m = do
      c <- wrapper m
      (ev1, _) <- headTailE (_chart_finished c)
      pure ()
    wrapper m = elAttr "div" ("style" =: "padding: 0px;") m

basicVerticalBarChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     , ToDouble a
     )
  => [(Text, a)] -> m (Chart t)
basicVerticalBarChart chartData = do
  let chartDataDyn = ((0 :: Int) =: (series, yAxisData, xAxisDyn))
      series = def
        & series_smooth ?~ Left True
        -- & series_areaStyle ?~ def

  barChart (BarChartConfig (300, 200)
              (constDyn basicBarChartOpts)
              chartDataDyn
            )
  where
    yAxisData = constDyn $ M.fromList $ fmap (DataDouble . toDouble) <$> chartData
    xAxisData = fst <$> chartData
    xAxisDyn = constDyn xAxisData
    basicBarChartOpts :: ChartOptions
    basicBarChartOpts = def
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_data ?~ (zip xAxisData $ repeat Nothing)
        ) : []

basicHorizontalBarChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     , ToDouble a
     )
  => [(Text, a)] -> m (Chart t)
basicHorizontalBarChart chartData = do
  let chartDataDyn = ((0 :: Int) =: (series, xAxisData, yAxisDyn))
      series = def
        & series_smooth ?~ Left True
        -- & series_areaStyle ?~ def

  barChart (BarChartConfig (300, 200)
              (constDyn basicBarChartOpts)
              chartDataDyn
            )
  where
    xAxisData = constDyn $ M.fromList $ fmap (DataDouble . toDouble) <$> chartData
    yAxisData = fst <$> chartData
    yAxisDyn = constDyn yAxisData
    basicBarChartOpts :: ChartOptions
    basicBarChartOpts = def
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Value
        ) : []
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_data ?~ (zip yAxisData $ repeat Nothing)
        ) : []

basicVerticalTimeBarChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     )
  => Text
  -> Event t [(Text, Double)]
  -> m (Chart t)
basicVerticalTimeBarChart title ev = do
  let
    chartData = (0 :: Int) =: (def, ev)
    opts :: ChartOptions
    opts = def
      & chartOptions_title ?~ (def
        & title_text ?~ title)
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        & axis_max ?~ Right ()
                              ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category) : []
  timeBarChart $ TimeBarChartConfig (300, 200) (constDyn opts)
    chartData


basicHorizontalTimeBarChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     )
  => Text
  -> Event t [(Text, Double)]
  -> m (Chart t)
basicHorizontalTimeBarChart title ev = do
  let
    chartData = (0 :: Int) =: (def, ev)
    opts :: ChartOptions
    opts = def
      & chartOptions_title ?~ (def
        & title_text ?~ title)
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        & axis_max ?~ Right ()
                              ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category) : []
  timeBarChart $ TimeBarChartConfig (300, 200) (constDyn opts)
    chartData

scatterPlotWidget
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => [(Text, Double)]
  -> m ()
scatterPlotWidget chartData = do
  prerender_ blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
    delayedRender $ basicScatterPlot chartData
    pure ()
  where
    delayedRender m = do
      c <- wrapper m
      (ev1, _) <- headTailE (_chart_finished c)
      pure ()
    wrapper m = elAttr "div" ("style" =: "padding: 0px;") m

basicScatterPlot
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => [(Text, Double)] -> m (Chart t)
basicScatterPlot chartData = do
  let chartDataDyn = ((0 :: Int) =: (series, yAxisData, xAxisDyn))
      series :: Series SeriesScatter
      series = def
        & series_smooth ?~ Left True
        -- & series_areaStyle ?~ def

  scatterChart (ScatterChartConfig (300, 200)
                 (constDyn basicScatterPlotOpts)
                 chartDataDyn
               )
  where
    yAxisData = constDyn $ M.fromList $ fmap DataDouble <$> chartData
    xAxisData = fst <$> chartData
    xAxisDyn = constDyn xAxisData
    basicScatterPlotOpts :: ChartOptions
    basicScatterPlotOpts = def
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_data ?~ (zip xAxisData $ repeat Nothing)
        ) : []