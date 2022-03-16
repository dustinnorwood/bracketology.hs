{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import           Backend.Config
import           Backend.Database
import           Backend.Ingestion
import           Backend.Monad
import           Common.Api.Performances.Performance
import           Common.Api.Players.Player
import           Common.Api.Teams.Team
import           Common.Api.Teams.TeamObject
import           Common.Api.Teams.TeamRankings
import           Common.Bracket
import           Common.Route
import           Common.Simulation.Details       (createTeamSimulationDetails)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.FT
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Dependent.Sum              (DSum ((:=>)))
import           Data.Function                   (on)
import           Data.Functor.Identity           (Identity (..))
import           Data.IORef
import           Data.List                       (sortBy)
import qualified Data.Map.Strict                 as M
import qualified Data.Map.Ordered                as O
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.Pool
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import           Data.Traversable                (for)

import           Database.PostgreSQL.Simple      (Connection, close)
import           Obelisk.Backend                 (Backend (..))
import           Obelisk.ExecutableConfig.Lookup
import           Obelisk.Route                   hiding (decode, encode)
import           Prelude                         hiding (lookup)
import           Snap
import           System.Random

getYolo :: Text -> IO Text
getYolo l = do
  configs <- liftIO $ getConfigs
  let l' = "backend/" <> l
  pure $ fromMaybe (error . T.unpack $ "Please fill in config: config/backend/" <> l') $
                T.decodeUtf8 <$> M.lookup l' configs

runApp :: IORef (M.Map Text TeamObject) -> AppMemServerM a -> Snap a
runApp = runAppMemServerM . AppMemServerEnv

lorem :: Text
lorem = T.concat
  [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse augue ligula, laoreet eget lectus id, luctus lacinia sem. Ut urna tortor, imperdiet ac arcu pulvinar, porta viverra neque. Vestibulum pharetra risus non vestibulum gravida. Nam quam metus, ornare sit amet massa nec, ullamcorper cursus felis. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Fusce vulputate massa ex, eu fermentum risus sodales at. In vel nibh a urna varius aliquam. Quisque venenatis, augue eget suscipit interdum, nisl nisi finibus libero, sed gravida eros lacus vitae justo. Aenean diam eros, sollicitudin vitae vestibulum in, rutrum finibus sapien. Phasellus in tellus mi. Proin iaculis leo eget urna varius sagittis. In iaculis lacinia felis, sit amet malesuada ante pretium in. Donec a ultricies odio. Phasellus laoreet, nunc blandit ultricies auctor, turpis ipsum aliquet nisl, vitae commodo nibh nisi in ante."
  , ""
  , "Aliquam ullamcorper elit porta eros vestibulum, id sagittis turpis dignissim. Quisque eget sem a mauris viverra ullamcorper at non ante. In hac habitasse platea dictumst. Donec fringilla convallis libero, et ullamcorper eros consectetur vulputate. Pellentesque lacinia nibh at purus ultrices, sit amet interdum lectus blandit. In ut pharetra nibh, ut ullamcorper urna. Fusce ac dui bibendum, vulputate ex in, rutrum purus. Aenean a urna elit. Proin vel iaculis tortor, ut semper felis. Cras ultricies eros nisi, ornare eleifend erat consectetur pharetra."
  , ""
  , "Pellentesque sit amet interdum neque. Etiam accumsan rutrum orci, ullamcorper posuere dolor maximus vitae. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Duis fringilla velit nec egestas egestas. Integer eget nisi tellus. Nunc eu nisl at purus volutpat pharetra. Nulla ut purus ut arcu placerat pellentesque eu quis ipsum. Integer vestibulum augue ut porttitor bibendum. Suspendisse quis volutpat est, nec auctor justo. Fusce vulputate convallis sapien, eget molestie enim aliquet in. Proin ligula odio, eleifend ut fermentum non, blandit id mauris. Ut augue tortor, auctor sed ornare vel, molestie quis arcu. Fusce porttitor odio sit amet nibh iaculis tincidunt. Vivamus eget lectus elementum, maximus mi sit amet, elementum arcu. Etiam congue vehicula dolor et viverra. Curabitur imperdiet neque sit amet elit suscipit fringilla."
  , ""
  , "Nam viverra sodales lorem, nec sollicitudin mi condimentum a. Proin elementum pellentesque felis sit amet fermentum. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Duis molestie, sapien in porta finibus, magna nibh interdum nisl, in dapibus dolor augue et lacus. Nam ut ante non lacus eleifend porta. Etiam aliquam nunc est, in consectetur diam vulputate eu. Cras egestas, lacus quis interdum aliquet, odio ipsum placerat sem, at tincidunt urna eros quis lacus. Ut venenatis euismod arcu, eget aliquam tellus tristique a. Maecenas blandit at leo sed condimentum. Quisque efficitur eget lacus ut accumsan. Integer consequat porta libero, eget faucibus metus tristique non."
  , ""
  , "Donec pellentesque at arcu quis tempor. Fusce mauris lorem, consectetur vel arcu eget, gravida tincidunt dolor. Ut sit amet consectetur mauris, eu suscipit tellus. Morbi quis erat arcu. Suspendisse cursus dignissim dolor et venenatis. Ut hendrerit magna ac tellus iaculis iaculis a vitae mi. Suspendisse interdum in dolor mattis fringilla. Proin ut leo elementum, lacinia leo sed, pharetra enim. Pellentesque sed mi elit. Maecenas mattis commodo lectus, in ullamcorper eros mollis eget. Donec fermentum nulla nisl, vitae condimentum urna lobortis eget. Proin molestie nibh felis, ac dictum quam placerat maximus. Phasellus malesuada sed quam ut interdum."
  ]

--pkgs :: M.Map Text TeamObject
--pkgs = M.fromList
--          [("UCLA Bruins", TeamObject
--            { _team = Team
--                "https://basketball.realgm.com/ncaa/conferences/Pacific-12-Conference/7/UCLA/241/home"
--                "UCLA Bruins"
--            , _matchups = O.empty
--            , _players = M.empty 
--            , _performances = M.empty
--            }
--          )
--          ]

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> do
      pgConnStr <- getYolo "pgConnStr"
      liftIO $ putStrLn "About to test the db connection. If ob run dies, check out config/backend/pgConnStr"
--      env <- mkEnv pgConnStr
--      conn <- openAppDb (T.encodeUtf8 pgConnStr)
--      eRes <- runReaderT (runExceptT downloadNewData) conn
--      liftIO $ putStrLn $ show eRes
      cfg <- readBackendConfig
      liftIO $ T.putStrLn $ "routeEnv: " <> _backendConfig_routeEnv cfg
      ePkgs <- parseTeamObjectsFromFile "/home/dustin/haskell/bracketology.hs/static/Teams.txt"
      case ePkgs of
        Left e -> liftIO $ putStrLn $ "Error parsing TeamObjects: " ++ show e
        Right pkgs -> do
          let pkgs' = getTeamRawScores pkgs
              pkgs'' = foldr ($) pkgs' $ replicate 20 adjustTeamScores
              teamScore = (^. team . team_score) <$> pkgs''
          pkgsRef <- newIORef pkgs''
          liftIO $ putStrLn $ "Finished ranking teams"
          serve $ \case
            BackendRoute_Missing :=> Identity () -> do
              writeLBS "404"
            BackendRoute_Api :/ apiR  -> do
              runApp pkgsRef $ case apiR of
                ApiRoute_Teams :/ teamsR -> case teamsR of
                  TeamsRoute_Get :/ w -> do
                    pkgs' <- fromMaybe M.empty <$> select @(M.Map Text Team) w
                    writeLBS $ encode pkgs'
                  TeamsRoute_Search :/ sp -> do
                    pkgs <- maybe M.empty unPrefixed <$> select @(Prefixed (M.Map Text Team)) sp
                    writeLBS $ encode pkgs
                ApiRoute_Team :/ teamR -> case teamR of
                  TeamRoute_Get :/ teamName -> do
                    liftIO . putStrLn . T.unpack $ "Getting team " <> unTeamName teamName
                    mTeam <- select @TeamObject teamName
                    case mTeam of
                      Nothing -> do
                        liftIO . putStrLn . T.unpack $ "Got Nothing"
                        writeLBS "404"
                      Just t -> do
                        liftIO . putStrLn . T.unpack $ "Got Just"
                        writeLBS $ encode t
                  TeamRoute_GetTeamNames :/ () -> do
                    writeLBS . encode $ M.keys pkgs
                ApiRoute_Player :/ (teamName, playerName) -> do
                  liftIO . putStrLn . T.unpack $ "Getting player " <> unPlayerName playerName <> " on team " <> unTeamName teamName
                  mPlayer <- select @(Player, [Performance]) (teamName, playerName)
                  case mPlayer of
                    Nothing -> do
                      liftIO . putStrLn . T.unpack $ "Got Nothing"
                      writeLBS "404"
                    Just t -> do
                      liftIO . putStrLn . T.unpack $ "Got Just"
                      writeLBS $ encode t
                ApiRoute_Matchup :/ matchupR -> case matchupR of
                  MatchupRoute_Run :/ teamNames -> case teamNames of
                    (team1:team2:_) -> do
                      mTeam1 <- select @TeamObject (TeamName team1)
                      case mTeam1 of
                        Nothing -> do
                          liftIO . putStrLn . T.unpack $ "Got Nothing"
                          writeLBS "404"
                        Just t1 -> do
                          liftIO . putStrLn . T.unpack $ "Got Just"
                          mTeam2 <- select @TeamObject (TeamName team2)
                          case mTeam2 of
                            Nothing -> do
                              liftIO . putStrLn . T.unpack $ "Got Nothing"
                              writeLBS "404"
                            Just t2 -> do
                              let tsd1 = createTeamSimulationDetails pkgs t1
                                  tsd2 = createTeamSimulationDetails pkgs t2
                              liftIO . putStrLn . T.unpack $ "Got Just"
                              writeLBS $ encode (tsd1,tsd2)
                ApiRoute_Bracket :/ () -> do
                  tsds <- for bracket2018 $ \teamName -> select @TeamObject (TeamName teamName) >>= \case
                    Nothing -> pure Nothing
                    Just t -> pure . Just $ createTeamSimulationDetails pkgs t
                  case sequence tsds of
                    Nothing -> writeLBS "404"
                    Just teams -> writeLBS $ encode teams
            BackendRoute_Api :=> _ -> pure ()
  }
