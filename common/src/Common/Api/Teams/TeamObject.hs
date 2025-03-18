{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Api.Teams.TeamObject where

import Control.Lens hiding ((.=), Context, (??), noneOf)

import           Control.Monad.FT.Alter
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.State
import           Data.Aeson
import           Data.Functor                     (($>))
import qualified Data.Map.Ordered                 as O
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Prelude                          hiding (lookup)
import           Text.Parsec


-- import           Backend.Claim                     (Claim (Claim), deriveToken)
import           Common.Api                        as Api
import           Common.Api.Teams.Team
import           Common.Api.Players.Player
import           Common.Api.Matchups.Matchup
import           Common.Api.Performances.Performance
import           Common.Route

data TeamObject = TeamObject
  { _team :: Team
  , _matchups :: O.OMap Text Matchup
  , _players  :: Map Text Player
  , _performances :: Map (Text, Text) Performance
  } deriving (Eq, Show)
makeLenses ''TeamObject

instance ToJSON TeamObject where
  toJSON TeamObject{..} = object
    [ "team" .= _team
    , "matchups" .= O.assocs _matchups
    , "players" .= _players
    , "performances" .= _performances
    ]

instance FromJSON TeamObject where
  parseJSON (Object o) = TeamObject
    <$> (o .: "team")
    <*> (O.fromList <$> (o .: "matchups"))
    <*> (o .: "players")
    <*> (o .: "performances")
  parseJSON o = fail $ "Could not parse TeamObject. Expected Object, got: " ++ show o

getOpposingMatchup :: Map Text TeamObject -> Text -> Matchup -> Maybe (Team, Maybe Matchup)
getOpposingMatchup teams teamName m = case M.lookup (m ^. matchup_opponent) teams of
  Nothing -> Nothing
  Just TeamObject{..} ->
    let m' = O.lookup (getOppMatchupId teamName m) _matchups
     in Just (_team, m')

type TOP = Parsec Text ()

semi :: TOP ()
semi = void $ char ';'

whitespace :: TOP ()
whitespace = void $ many $ oneOf [' ','\t','\r','\n']

textField :: TOP Text
textField = T.pack <$> many (noneOf [';','\r','\n'])

number :: TOP Integer
number = read <$> many digit

boolean :: TOP Bool
boolean = (string "True" $> True) <|> (string "False" $> False)

parseTeamObjectsFromFile :: MonadIO m => String -> m (Either String (Map Text TeamObject))
parseTeamObjectsFromFile fname = do
  input <- liftIO $ readFile fname
  let eTeamObsList = traverse (eitherDecodeStrict . encodeUtf8 . T.pack) $ lines input
  liftIO $ putStrLn $ "Finished parsing TeamObjects"
  pure $ M.fromList . map (\o -> (o ^. team . team_name, o)) <$> eTeamObsList

runTeamObjectParser :: SourceName -> Text -> Either ParseError (Map Text TeamObject)
runTeamObjectParser = runParser parseTeamObjects ()

parseTeamObjects :: TOP (Map Text TeamObject)
parseTeamObjects = do
  ts <- many parseTeamObject
  pure $ M.fromList $ (\o -> (o ^. team . team_name, o)) <$> ts

parseTeamObject :: TOP TeamObject
parseTeamObject = do
  t <- parseTeam
  ms <- many (parseMatchup $ t ^. team_name)
  ~(players, perfs) <- fmap concat . unzip <$> many (parsePlayer $ t ^. team_name)
  let matchupMap = O.fromList $ (\m -> (m ^. matchup_id, m)) <$> ms
      playerMap = M.fromList $ (\p -> (p ^. player_name, p)) <$> players
      perfMap = M.fromList $ (\p -> ((p ^. performance_matchupId, p ^. performance_player), p)) <$> perfs
  pure $ TeamObject t matchupMap playerMap perfMap

parseTeam :: TOP Team 
parseTeam = do
  string "Team:"
  tName <- textField
  semi
  tUrl <- textField
  whitespace
  pure $ Team tUrl tName "" 0.0

parseMatchup :: Text -> TOP Matchup
parseMatchup teamName = do
  string "Matchup:"
  opp <- textField
  semi
  win <- boolean
  semi
  home <- boolean
  semi
  tScore <- number
  semi
  oScore <- number
  semi
  mId <- textField
  whitespace
  pure $ Matchup teamName opp win home tScore oScore mId

parsePlayer :: Text -> TOP (Player, [Performance]) 
parsePlayer teamName = do
  string "Player:"
  name <- textField
  semi
  c <- textField
  semi
  bd <- textField
  semi
  bc <- textField
  semi
  nat <- textField
  semi
  hs <- textField
  semi
  gp <- number
  semi
  h <- number
  semi
  w <- number
  semi
  n <- number
  semi
  pos <- textField
  semi
  whitespace
  let player = Player name teamName pos gp h w n c bd bc nat hs
  perfs <- many $ parsePerformance name
  pure (player, perfs)

parsePerformance :: Text -> TOP Performance
parsePerformance pName = do
  string "Zerformance:"
  pts <- number
  semi
  or' <- number
  semi
  dr <- number
  semi
  as <- number
  semi
  ss <- number
  semi
  bs <- number
  semi
  ts <- number
  semi
  pf <- number
  semi
  fgm <- number
  semi
  fga <- number
  semi
  ftm <- number
  semi
  fta <- number
  semi
  tpm <- number
  semi
  tpa <- number
  semi
  mins <- textField
  semi
  fic <- textField
  semi
  status <- textField
  semi
  mId <- textField
  whitespace
  pure $ Performance mId pName status pts or' dr as ss bs ts fgm fga ftm fta tpm tpa pf mins fic