{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Common.Simulation where

import           Control.Lens
import           Control.Monad               (void, when)
import           Control.Monad.FT.Modify     (Gettable, Puttable, Modifiable)
import qualified Control.Monad.FT.Modify     as FT
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.State
import           Data.Bool                   (bool)
import           Data.Function               (on)
import           Data.List                   (sortBy)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe)
import           Data.Ord                    (Down(..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.Random
import           Common.Api.Players.Player
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Logging
import           Common.Probability
import           Common.Simulation.Details
import           Common.Statistics.Player
import           Common.Statistics.Util

data PossessionOutcome = ShotMade | Rebound | Turnover

data TeamSimulation = TS
  { _ts_team               :: TeamSimulationDetails
  , _ts_team_score         :: Int
  , _ts_team_ratio         :: Double
  , _ts_playersOnCourt     :: [PlayerProbability]
  , _ts_playerWithBall     :: Maybe PlayerProbability
  } deriving (Show)
makeLenses ''TeamSimulation

data SimulationData = SD
  { _sd_possession          :: Bool
  , _sd_period              :: Int
  , _sd_shotClock           :: Double
  , _sd_secondsLeftInPeriod :: Double
  , _sd_team1               :: TeamSimulation
  , _sd_team2               :: TeamSimulation
  , _sd_stdgen              :: StdGen
  } deriving (Show)
makeLenses ''SimulationData

instance Monad m => HasRandom (StateT SimulationData m) where
  randomBool = do
    stdgen <- use sd_stdgen
    let (b, stdgen') = random stdgen
    sd_stdgen .= stdgen'
    pure b
  randomDouble = do
    stdgen <- use sd_stdgen
    let (d, stdgen') = randomR (0.0, 1.0) stdgen
    sd_stdgen .= stdgen'
    pure d

instance Puttable LogMessage m => Puttable LogMessage (StateT SimulationData m) where
  put = lift . FT.put

startingSimulationData :: TeamSimulationDetails
                       -> TeamSimulationDetails
                       -> Int
                       -> SimulationData
startingSimulationData tsd1 tsd2 seed =
  let s1 = 1 + tsd1 ^. tsd_teamObj . team . team_score
      s2 = 1 + tsd2 ^. tsd_teamObj . team . team_score
      r1 = s1 / s2
      r2 = s2 / s1
      ts1 = TS tsd1 0 r1 [] Nothing
      ts2 = TS tsd2 0 r2 [] Nothing
   in SD
        { _sd_possession          = False
        , _sd_period              = 1
        , _sd_shotClock           = 35.0
        , _sd_secondsLeftInPeriod = 60.0 * 20.0
        , _sd_team1               = ts1
        , _sd_team2               = ts2
        , _sd_stdgen              = mkStdGen seed
        }

constants_shotClock :: Double
constants_shotClock = 35.0

resetShotClock :: Monad m => StateT SimulationData m ()
resetShotClock = sd_shotClock .= constants_shotClock

setStarters :: HasRandom m => TeamSimulation -> m TeamSimulation
setStarters ts = do
  let lineup = sortBy (compare `on` (Down . _pp_startProbability)) . M.elems $ ts ^. ts_team . tsd_playerProbabilities
      isOnCourt p = (,p) <$> bernoulli (_pp_onCourtProbability p)
      starters = take 5 lineup -- . map snd . filter fst <$> traverse isOnCourt lineup
  pure $ (ts_playersOnCourt .~ starters) ts

setPlayerWithBall :: HasRandom m => TeamSimulation -> m TeamSimulation
setPlayerWithBall ts = do
  d <- randomDouble
  let ps = ts ^. ts_playersOnCourt
      i = getCdfBin (zipWith const (repeat 0.2) ps) d
  case drop i ps of
    [] -> pure ts
    (x:_) -> pure $ (ts_playerWithBall ?~ x) ts

getPlayerWithBall :: TeamSimulation -> Maybe PlayerProbability
getPlayerWithBall = _ts_playerWithBall

playGame :: Puttable LogMessage m
         => TeamSimulationDetails
         -> TeamSimulationDetails
         -> Int
         -> m (SimulationData, SimulationData)
playGame tsd1 tsd2 seed = flip runStateT (startingSimulationData tsd1 tsd2 seed) $ do
  let t1name = tsd1 ^. tsd_teamObj . team . team_name
  let t2name = tsd2 ^. tsd_teamObj . team . team_name
  FT.put . LogMessage $ "Starting game between " <> t1name <> " and " <> t2name
  -- FT.put . LogMessage . T.pack $ show tsd1
  -- FT.put . LogMessage . T.pack $ show tsd2
  t1 <- use sd_team1
  t1' <- setStarters t1
  sd_team1 .= t1'
  t2 <- use sd_team2
  t2' <- setStarters t2
  sd_team2 .= t2'
  FT.put . LogMessage $ "Starters for " <> t1name <> ":\n"
                  <> T.pack (show $ (^. pp_player . player_name) <$> t1' ^. ts_playersOnCourt)
  FT.put . LogMessage $ "Starters for " <> t2name <> ":\n"
                  <> T.pack (show $ (^. pp_player . player_name) <$> t2' ^. ts_playersOnCourt)
  sd <- get
  playPeriod 1
  playPeriod 2
  playOvertime
  pure sd


playPeriod :: Puttable LogMessage m => Int -> StateT SimulationData m ()
playPeriod n = do
  tsd1 <- use sd_team1
  tsd2 <- use sd_team2
  let t1name = tsd1 ^. ts_team . tsd_teamObj . team . team_name
  let t2name = tsd2 ^. ts_team . tsd_teamObj . team . team_name
  FT.put . LogMessage $ "Playing period " <> T.pack (show n)
  possession <- randomBool
  sd_possession .= possession
  FT.put . LogMessage $ (if possession then t2name else t1name) <> " won tip-off"
  sd_period .= n
  let seconds = fromInteger $ 60 * (if n < 3 then 20 else 5)
  sd_secondsLeftInPeriod .= seconds
  resetShotClock
  playPeriodPossessions

playOvertime :: Puttable LogMessage m => StateT SimulationData m ()
playOvertime = do
  t1Score <- use $ sd_team1 . ts_team_score
  t2Score <- use $ sd_team2 . ts_team_score
  when (t1Score == t2Score && t1Score /= 0) $ do
    n <- use sd_period
    playPeriod $ n + 1
    playOvertime

playPeriodPossessions :: Puttable LogMessage m => StateT SimulationData m ()
playPeriodPossessions = do
  t1Score <- use $ sd_team1 . ts_team_score
  t2Score <- use $ sd_team2 . ts_team_score
  FT.put . LogMessage $ "Score: " <> T.pack (show t1Score) <> " - " <> T.pack (show t2Score)
  secs <- use sd_secondsLeftInPeriod
  FT.put . LogMessage $ "Clock: " <> T.pack (show . floor $ secs / 60.0) <> ":" <> T.pack (show $ (round secs) `mod` 60)
  if secs < 0.0
    then pure ()
    else do
      pos <- use sd_possession
      sd_possession %= not
      pos' <- use sd_possession
      resetShotClock
      playPossession
      playPeriodPossessions

teamWithBall :: Puttable LogMessage m => StateT SimulationData m TeamSimulation
teamWithBall = do
  pos <- use sd_possession
  if pos
    then use sd_team1
    else use sd_team2

scorePoints :: Puttable LogMessage m => Int -> StateT SimulationData m ()
scorePoints n = do
  pos <- use sd_possession
  if pos
    then sd_team1 . ts_team_score += n
    else sd_team2 . ts_team_score += n

teamWithoutBall :: Puttable LogMessage m => StateT SimulationData m TeamSimulation
teamWithoutBall = do
  pos <- use sd_possession
  if pos
    then use sd_team2
    else use sd_team1

playPossession :: Puttable LogMessage m => StateT SimulationData m ()
playPossession = do
  selectPlayerWithBall
  pos <- use sd_possession
  tsd1 <- use $ sd_team1 . ts_team
  tsd2 <- use $ sd_team2 . ts_team
  let t1name = tsd1 ^. tsd_teamObj . team . team_name
  let t2name = tsd2 ^. tsd_teamObj . team . team_name
  twb <- teamWithBall
  isTurnover twb >>= \case
    True -> do
      FT.put . LogMessage $ "Turnover"
      void removeSeconds
    False -> foulRoutine False >>= \case
      True -> do
        FT.put . LogMessage $ "Non-shooting foul"
        freeThrows <- pure 2 -- getNumFreeThrows
        _ <- traverse (const freeThrowRoutine) $ replicate freeThrows ()
        pure ()
      False -> isPass twb >>= \case
        True -> removeSeconds >>= \case
          True -> do
            FT.put . LogMessage $ "Ball pass"
            playPossession
          False -> pure ()
        False -> removeSeconds >>= \case
          True -> shootBall >>= \case
            Rebound -> do
              reboundRoutine >>= \case
                True -> do
                  FT.put . LogMessage $ "Offensive rebound"
                  resetShotClock >> playPossession
                False -> do
                  FT.put . LogMessage $ "Defensive rebound"
                  pure ()
            _ -> pure ()
          False -> pure ()

selectPlayerWithBall :: Puttable LogMessage m => StateT SimulationData m ()
selectPlayerWithBall = do
  pos <- use sd_possession
  if pos
    then do
      t1 <- use sd_team1
      t1' <- setPlayerWithBall t1
      let shooter = t1' ^. ts_playerWithBall . _Just . pp_player . player_name
      FT.put . LogMessage $ shooter <> " has the ball"
      sd_team1 .= t1'
    else do
      t2 <- use sd_team2
      t2' <- setPlayerWithBall t2
      let shooter = t2' ^. ts_playerWithBall. _Just . pp_player . player_name
      FT.put . LogMessage $ shooter <> " has the ball"
      sd_team2 .= t2'

removeSeconds :: Puttable LogMessage m => StateT SimulationData m Bool
removeSeconds = do
  n <- randomDouble
  sc <- use sd_shotClock
  let secondsToRemove = n * (sc + 1)
  sc' <- sd_shotClock <%= (\s -> s - secondsToRemove)
  slip' <- sd_secondsLeftInPeriod <%= (\s -> s - secondsToRemove)
  pure $ sc' > 0.0 && slip' > 0.0

freeThrowRoutine :: Puttable LogMessage m => StateT SimulationData m PossessionOutcome
freeThrowRoutine = do
  t <- teamWithBall
  isFreeThrowMade t >>= \case
    True -> do
      FT.put . LogMessage $ "Free throw made"
      ShotMade <$ scorePoints 1
    False -> do
      FT.put . LogMessage $ "Free throw missed"
      pure Rebound

shootBall :: Puttable LogMessage m => StateT SimulationData m PossessionOutcome
shootBall = foulRoutine True >>= \case
  True -> shootWithFoul
  False -> do
    twob <- teamWithoutBall
    isBlock twob >>= \case
      True -> do
        FT.put . LogMessage $ "Shot blocked"
        pure Turnover
      False -> shootWithoutFoul

shootWithFoul :: Puttable LogMessage m => StateT SimulationData m PossessionOutcome
shootWithFoul = do
  twb <- teamWithBall
  isShootingThree twb >>= \case
    True -> do
      FT.put . LogMessage $ "Three pointer attempted"
      isThreePointShotMade twb >>= \case
        True -> do
          FT.put . LogMessage $ "Three point shot made"
          scorePoints 3
          freeThrowRoutine
        False -> do
          FT.put . LogMessage $ "Three point shot missed"
          void freeThrowRoutine
          void freeThrowRoutine
          freeThrowRoutine
    False -> do
      FT.put . LogMessage $ "Two point shot attempted"
      isTwoPointShotMade twb >>= \case
        True -> do
          FT.put . LogMessage $ "Two point shot made"
          scorePoints 2
          freeThrowRoutine
        False -> do
          FT.put . LogMessage $ "Two point shot missed"
          void freeThrowRoutine
          freeThrowRoutine

shootWithoutFoul :: Puttable LogMessage m => StateT SimulationData m PossessionOutcome
shootWithoutFoul = do
  twb <- teamWithBall
  isShootingThree twb >>= \case
    True -> do
      FT.put . LogMessage $ "Three pointer attempted"
      isThreePointShotMade twb >>= \case
        True -> do
          FT.put . LogMessage $ "Three point shot made"
          scorePoints 3
          pure ShotMade
        False -> do
          FT.put . LogMessage $ "Three point shot missed"
          pure Rebound
    False -> do
      FT.put . LogMessage $ "Two point shot attempted"
      isTwoPointShotMade twb >>= \case
        True -> do
          FT.put . LogMessage $ "Two point shot made"
          scorePoints 2
          pure ShotMade
        False -> do
          FT.put . LogMessage $ "Two point shot missed"
          pure Rebound

foulRoutine :: Puttable LogMessage m => Bool -> StateT SimulationData m Bool
foulRoutine isShootingFoul = do
  twob <- teamWithoutBall
  if isShootingFoul
    then bernoulli $ twob ^. ts_team . tsd_foulPercentage * twob ^. ts_team . tsd_teamObj . team . team_score
    else do
      twb <- teamWithBall
      foulUrgency <-
        if twob ^. ts_team_score >= twb ^. ts_team_score
          then pure 1.0
          else do
            period <- use sd_period
            seconds <- use sd_secondsLeftInPeriod
            pure $ if period >= 2 && seconds < 120
              then (((toDouble $ twb ^. ts_team_score - twob ^. ts_team_score)*(150.0 - seconds)))/120.0
              else 1.0
      bernoulli $ twob ^. ts_team . tsd_foulPercentage * twob ^. ts_team . tsd_teamObj . team . team_score * foulUrgency


reboundRoutine :: Puttable LogMessage m => StateT SimulationData m Bool
reboundRoutine = do
  twb <- (^. ts_team) <$> teamWithBall
  twob <- (^. ts_team) <$> teamWithoutBall
  let orp = twb ^. tsd_offensiveReboundPercentage * twb ^. tsd_teamObj . team . team_score
      twbScore = twb ^. tsd_teamObj . team . team_score
      drp = twob ^. tsd_defensiveReboundPercentage * twob ^. tsd_teamObj . team . team_score
      twobScore = twob ^. tsd_teamObj . team . team_score
  n <- randomDouble
  pure $ getCdfBin [orp * twbScore, drp * twobScore] n == 0

bernoulli :: HasRandom m => Double -> m Bool
bernoulli p = do
  d <- randomDouble
  pure $ d < p

isTwoPointShotMade :: HasRandom m => TeamSimulation -> m Bool
isTwoPointShotMade ts = case ts ^. ts_playerWithBall of
  Nothing -> pure False
  Just p -> bernoulli $ p ^. pp_fieldGoalPercentage * ts ^. ts_team_ratio

isThreePointShotMade :: HasRandom m => TeamSimulation -> m Bool
isThreePointShotMade ts = case ts ^. ts_playerWithBall of
  Nothing -> pure False
  Just p -> bernoulli $ p ^. pp_threePointPercentage * ts ^. ts_team_ratio

isFreeThrowMade :: HasRandom m => TeamSimulation -> m Bool
isFreeThrowMade ts = case ts ^. ts_playerWithBall of
  Nothing -> pure False
  Just p -> bernoulli $ (p ^. pp_freeThrowPercentage) * (ts ^. ts_team . tsd_teamObj . team . team_score)

isShootingThree :: HasRandom m => TeamSimulation -> m Bool
isShootingThree ts = case ts ^. ts_playerWithBall of
  Nothing -> pure False
  Just p -> bernoulli $ p ^. pp_threePointProbability

isPass :: HasRandom m => TeamSimulation -> m Bool
isPass ts = case ts ^. ts_playerWithBall of
  Nothing -> pure False
  Just p -> bernoulli $ p ^. pp_assistPercentage * ts ^. ts_team_ratio

isTurnover :: HasRandom m => TeamSimulation -> m Bool
isTurnover ts = case ts ^. ts_playerWithBall of
  Nothing -> pure False
  Just p -> bernoulli $ p ^. pp_turnoverPercentage / ts ^. ts_team_ratio

isBlock :: HasRandom m => TeamSimulation -> m Bool
isBlock ts = bernoulli $ ts ^. ts_team . tsd_blockPercentage * ts ^. ts_team_ratio

isFoul :: HasRandom m => TeamSimulation -> m Bool
isFoul ts = bernoulli $ ts ^. ts_team . tsd_foulPercentage

type MuSigma = (Double, Double)

type Normal f = f MuSigma
type Outcome f = f Double

data PlayerSim a = PS
  { _ps_mins :: a
  , _ps_fg   :: a
  , _ps_ft   :: a
  , _ps_tp   :: a
  } deriving (Eq, Show)
makeLenses ''PlayerSim

newtype TeamSim a = TSS
  { _ts_pss :: M.Map Text (PlayerSim a)
  } deriving (Eq, Show)
makeLenses ''TeamSim

data Sim a = Sim
  { _sim_t1 :: a
  , _sim_t2 :: a
  } deriving (Eq, Show)
makeLenses ''Sim

getTeamSimDetails :: TeamSimulationDetails -> M.Map Text (Normal PlayerSim)
getTeamSimDetails tsd =
  let mins = M.map muSigma . minutesByGameByPlayer $ tsd ^. tsd_teamObj
      fgm = M.map muSigma . fieldGoalsMadeByGameByPlayer $ tsd ^. tsd_teamObj
      ftm = M.map muSigma . freeThrowsMadeByGameByPlayer $ tsd ^. tsd_teamObj
      tpm = M.map muSigma . threePointersMadeByGameByPlayer $ tsd ^. tsd_teamObj
      pss = M.mapWithKey (\k m -> PS
                           { _ps_mins = m
                           , _ps_fg = fromMaybe (0.0,0.0) $ M.lookup k fgm
                           , _ps_ft = fromMaybe (0.0,0.0) $ M.lookup k ftm
                           , _ps_tp = fromMaybe (0.0,0.0) $ M.lookup k tpm
                           }
                         ) mins
   in pss

traversePS :: Applicative f => (a -> f b) -> PlayerSim a -> f (PlayerSim b)
traversePS f (PS a b c d) = PS <$> f a <*> f b <*> f c <*> f d

instance Monad m => Gettable StdGen (StateT StdGen m) where
  get = get
instance Monad m => Puttable StdGen (StateT StdGen m) where
  put = put
instance Monad m => Modifiable StdGen (StateT StdGen m) where

normalOutcome :: ( Modifiable StdGen m
                 )
              => MuSigma
              -> m Double
normalOutcome = FT.modifyReturningPure @StdGen . normal'

simulateGame :: Puttable LogMessage m
             => TeamSimulationDetails
             -> TeamSimulationDetails
             -> Int
             -> m (Sim Double)
simulateGame tsd1 tsd2 seed = simDataToSim . snd <$> playGame tsd1 tsd2 seed

simDataToSim :: SimulationData -> Sim Double
simDataToSim sd = Sim (fromIntegral $ sd ^. sd_team1 . ts_team_score) (fromIntegral $ sd ^. sd_team2 . ts_team_score)
  -- flip evalStateT (mkStdGen seed) $ do
  -- let tss1 = getTeamSimDetails tsd1
  --     tss2 = getTeamSimDetails tsd2
  -- tss1' <- traverse (traversePS normalOutcome) tss1
  -- tss2' <- traverse (traversePS normalOutcome) tss2
  -- let p1 = sum $ points <$> tss1'
  -- let p2 = sum $ points <$> tss2'
  -- pure $ Sim p1 p2

points :: Num a => PlayerSim a -> a
points (PS _ fg ft tp) = 2*fg + ft + 3*tp