{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Backend.Ingestion where

import           Backend.Database                (AppDb (..), QueryError (..), appDb,
                                                   maybeRow, rowList, singleRow)
import qualified Backend.Database.Matchups     as Matchups
import qualified Backend.Database.Performances as Performances
import qualified Backend.Database.Players      as Players
import qualified Backend.Database.Teams        as Teams
import Common.Api.Matchups.Matchup
import Common.Api.Performances.Performance
import Common.Api.Players.Player
import Common.Api.Teams.Team
import Common.Api.Teams.TeamObject
import Control.Concurrent
import Control.Lens
import Control.Monad.Error.Class       (MonadError)
import Control.Monad.Fail              (MonadFail)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class      (MonadReader, ask)
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control     (MonadBaseControl)
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_, for_)
import qualified Data.Map.Ordered                 as O
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Traversable (for)
import Database.PostgreSQL.Simple      (Connection)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified System.IO as SIO
import Text.HTML.Parser
import Text.Parsec
import Text.Read (readMaybe)

type Html m = StateT [Token] m
-- newtype Html a = Html { runHtml :: MaybeT (State [Token]) a }
-- 
-- instance Functor Html where
--   fmap f (Html ma) = Html (fmap f a)
-- 
-- instance Applicative Html where
--   pure a = Html (pure a)
--   (Html fab) <*> (Html fa) = Html $ fab <*> fa
-- 
-- instance Monad Html where
--   return = pure
--   (Html ma) >>= amb = Html $ lift get >>= \case
--     [] -> pure Nothing
--     _ -> do
--       a <- ma
--       case amb a of
--         Html ma -> ma

tReadMaybe :: Read a => Text -> Maybe a
tReadMaybe = readMaybe . T.unpack

tagOpen :: Monad m => Text -> Html m ()
tagOpen tag = get >>= \case
  [] -> pure ()
  (TagOpen t _):ts | t == tag -> put ts
  (_:ts) -> put ts >> tagOpen tag

tagOpenLenient :: Monad m => Text -> Html m ()
tagOpenLenient tag = get >>= \case
  [] -> pure ()
  (TagOpen t _):ts | t == tag -> put ts
  (ContentText t):ts -> pure ()
  (ContentChar c):ts -> pure ()
  (_:ts) -> put ts >> tagOpenLenient tag

contentText :: Monad m => Html m (Maybe Text)
contentText = get >>= \case
  [] -> pure Nothing
  (ContentText t):ts -> put ts >> (Just . (t <>) <$> getRestOfContentText)
  (ContentChar c):ts -> put ts >> (Just . (T.pack [c] <>) <$> getRestOfContentText)
  (_:ts) -> put ts >> contentText
  where getRestOfContentText = get >>= \case
          (ContentText t):ts -> put ts >> ((t <>) <$> getRestOfContentText)
          (ContentChar c):ts -> put ts >> ((T.pack [c] <>) <$> getRestOfContentText)
          _ -> pure ""

tagContentText :: Monad m => Text -> Html m (Maybe Text)
tagContentText tag = tagOpen tag >> contentText

tagContentTextLenient :: Monad m => Text -> Html m (Maybe Text)
tagContentTextLenient tag = tagOpenLenient tag >> contentText

matchContentText :: Monad m => Text -> Html m ()
matchContentText t' = get >>= \case
  [] -> pure ()
  (ContentText t):ts | t == t' -> put ts
  (_:ts) -> put ts >> matchContentText t'

tagOpenWithAttr :: Monad m
                => Text -> Text -> Html m (Maybe Text)
tagOpenWithAttr tag attr = get >>= \case
  [] -> pure Nothing
  (TagOpen t attrs):ts | t == tag -> put ts >>
    case getAttr attr attrs of
      Nothing -> tagOpenWithAttr tag attr
      Just v -> pure $ Just v
  (TagSelfClose t attrs):ts | t == tag -> put ts >>
    case getAttr attr attrs of
      Nothing -> tagOpenWithAttr tag attr
      Just v -> pure $ Just v
  (_:ts) -> put ts >> tagOpenWithAttr tag attr

tagOpenWithAttrValue :: Monad m
                     => Text -> Text -> Text -> Html m ()
tagOpenWithAttrValue tag attr value = tagOpenWithAttr tag attr >>= \case
  Nothing -> pure ()
  Just val | val == value -> pure ()
  Just _ -> tagOpenWithAttrValue tag attr value

tr :: Monad m => Html m ()
tr = tagOpen "tr"

td :: Monad m => Html m ()
td = tagOpen "td"

tdClassNoWrap :: Monad m => Html m ()
tdClassNoWrap = tagOpenWithAttrValue "td" "class" "nowrap"

a :: Monad m => Html m ()
a = tagOpen "a"

aHref :: Monad m => Html m (Maybe Text)
aHref = tagOpenWithAttr "a" "href"

img :: Monad m => Html m (Maybe Text)
img = tagOpenWithAttr "img" "src"

rel :: Monad m => Html m (Maybe Text)
rel = get >>= \case
  [] -> pure Nothing
  (TagOpen _ attrs):ts -> put ts >>
    case getAttr "rel" attrs of
      Nothing -> rel
      Just v -> pure $ Just v
  _:ts -> put ts >> rel

getAttr :: Text -> [Attr] -> Maybe Text
getAttr _ [] = Nothing
getAttr t ((Attr t' v):_) | t == t' = Just v
getAttr t (_:attrs) = getAttr t attrs

downloadPerformances :: MonadIO m => Manager -> (Text,Matchup) -> m (Matchup, [Performance])
downloadPerformances manager (matchupUrl, m@Matchup{..}) = do
  liftIO . putStrLn $ "Downloading performances for " ++ T.unpack _matchup_id
  req <- liftIO . parseRequest $ T.unpack ("https://realgm.com" <> matchupUrl)
  res <- liftIO $ httpLbs req manager
  let tkns = either (const []) parseTokens . decodeUtf8' . LBS.toStrict $ responseBody res
      getPerformances = getPerformance _matchup_id >>= \case
        Nothing -> pure []
        Just performance -> (performance:) <$> getPerformances
  when (null tkns) $ liftIO . putStrLn $ "error decoding page"
  performances <- flip evalStateT tkns $ do
    tagOpen "h2"
    when _matchup_home $ do
      tagOpen "h2"
      tagOpen "tbody"
    tagOpen "h2"
    tagOpen "tbody"
    getPerformances
  pure (m, performances)

parseMadeAttempted :: Parsec Text () (Integer, Integer)
parseMadeAttempted = do
  made <- fromMaybe 0 . readMaybe <$> many digit
  char '-'
  attempted <- fromMaybe 0 . readMaybe <$> many digit
  pure (made, attempted)

madeAttempted :: Text -> Maybe (Integer, Integer)
madeAttempted = either (const Nothing) Just . runParser parseMadeAttempted () ""

getPerformance :: Monad m => Text -> Html m (Maybe Performance)
getPerformance matchupId = do
  tr
  td
  mName <- tagContentText "td"
  mStatus <- tagContentText "td"
  mPos <- tagContentText "td"
  mMin <- tagContentText "td"
  mFgmAT <- tagContentText "td"
  let mFgmA = madeAttempted =<< mFgmAT
  m3pmAT <- tagContentText "td"
  let m3pmA = madeAttempted =<< m3pmAT
  mFtmAT <- tagContentText "td"
  let mFtmA = madeAttempted =<< mFtmAT
  mFIC <- tagContentText "td"
  mOffR <- (tReadMaybe =<<) <$> tagContentText "td"
  mDefR <- (tReadMaybe =<<) <$> tagContentText "td"
  td
  mAst <- (tReadMaybe =<<) <$> tagContentText "td"
  mPF <- (tReadMaybe =<<) <$> tagContentText "td"
  mStl <- (tReadMaybe =<<) <$> tagContentText "td"
  mTO <- (tReadMaybe =<<) <$> tagContentText "td"
  mBlk <- (tReadMaybe =<<) <$> tagContentText "td"
  mPts <- (tReadMaybe =<<) <$> tagContentText "td"
  pure $ Performance
     <$> mName
     <*> Just matchupId
     <*> mStatus
     <*> mPts
     <*> mOffR
     <*> mDefR
     <*> mAst
     <*> mStl
     <*> mBlk
     <*> mTO
     <*> (fst <$> mFgmA)
     <*> (snd <$> mFgmA)
     <*> (fst <$> mFtmA)
     <*> (snd <$> mFtmA)
     <*> (fst <$> m3pmA)
     <*> (snd <$> m3pmA)
     <*> mPF
     <*> mMin
     <*> mFIC

downloadMatchupsAndPerformances :: MonadIO m => Manager -> Team -> m (Text, [(Matchup, [Performance])])
downloadMatchupsAndPerformances manager t = do
  liftIO . putStrLn $ "Downloading schedule for " ++ T.unpack (t ^. team_name)
  req <- liftIO . parseRequest . T.unpack $ (t ^. team_url) <> "/schedule"
  res <- liftIO $ httpLbs req manager
  let tkns = either (const []) parseTokens . decodeUtf8' . LBS.toStrict $ responseBody res
      getMatchups = getMatchup (t ^. team_name) >>= \case
        Nothing -> pure []
        Just matchup -> (matchup:) <$> getMatchups
  when (null tkns) $ liftIO . putStrLn $ "error decoding page"
  ~(teamImage, matchups) <- flip evalStateT tkns $ do
    tagOpenWithAttrValue "h2" "class" "page_title"
    image <- maybe "" ("https://basketball.realgm.com" <>) <$> img
    tagOpen "tbody"
    (image,) <$> getMatchups
  fmap (teamImage,) $ traverse (\m -> liftIO (threadDelay 1000000) >> downloadPerformances manager m) matchups

getMatchup :: Monad m => Text -> Html m (Maybe (Text, Matchup))
getMatchup teamName = do
  tr
  tagOpenWithAttrValue "td" "data-th" "Opponent"
  mHomeVal <- tagContentText "div"
  let mHome = (/="@") <$> mHomeVal
  a
  mOpponent <- tagContentTextLenient "a"
  tagOpenWithAttrValue "td" "data-th" "Result"
  mMatchupUrl <- aHref
  mResultT <- contentText
  let mResult = join
              $ either (const Nothing) Just
              . runParser parseMatchupResult () ""
            <$> mResultT
      mWin = (\(w,_,_) -> w) <$> mResult
      snd3 (_,b,_) = b
      thd3 (_,_,c) = c
      mTeamScore = (if fromMaybe False mHome then snd3 else thd3) <$> mResult
      mOppScore = (if fromMaybe False mHome then thd3 else snd3) <$> mResult
      mId = fmap T.concat $ sequence [
          mOpponent
        , T.pack . show <$> mTeamScore
        , T.pack . show <$> mOppScore
        ]
  pure $ (,)
     <$> mMatchupUrl
     <*> ( Matchup
       <$> Just teamName
       <*> mOpponent
       <*> mWin
       <*> mHome
       <*> mTeamScore
       <*> mOppScore
       <*> mId
       )

parseMatchupResult :: Parsec Text () (Bool, Integer, Integer)
parseMatchupResult = do
  win <- choice [True <$ char 'W', False <$ char 'L']
  string ", "
  homeScore <- fromMaybe 0 . readMaybe <$> many digit
  string " - "
  awayScore <- fromMaybe 0 . readMaybe <$> many digit
  pure (win, homeScore, awayScore)

downloadPlayers :: MonadIO m => Manager -> Team -> m [Player]
downloadPlayers manager t = do
  req <- liftIO . parseRequest . T.unpack $ (t ^. team_url) <> "/rosters"
  res <- liftIO $ httpLbs req manager
  let tkns = either (const []) parseTokens . decodeUtf8' . LBS.toStrict $ responseBody res
      getPlayers = getPlayer (t ^. team_name) >>= \case
        Nothing -> pure []
        Just player -> (player:) <$> getPlayers
  when (null tkns) $ liftIO . putStrLn $ "error decoding page"
  flip evalStateT tkns $ do
    matchContentText "High School/Prep School"
    getPlayers

getPlayer :: MonadIO m => Text -> Html m (Maybe Player)
getPlayer teamName = do
  tr
  mNumberT <- tagContentText "td"
  let mNumber = tReadMaybe =<< mNumberT
  mName <- tagContentText "a"
  mClass <- tagContentText "td"
  mPosition <- tagContentText "td"
  let getHeight h = do
        let hs = T.split (=='-') h
            ~(mFt, mInches) = case hs of
              [] -> (Nothing, Nothing)
              [x] -> (Just x, Nothing)
              (f:i:_) -> (Just f, Just i)
        ft <- tReadMaybe =<< mFt
        inches <- tReadMaybe =<< mInches
        pure $ 12 * ft + inches
  mHeightT <- tagContentText "td"
  let mHeight = Just . fromMaybe 72 . getHeight =<< mHeightT
  mWeightT <- tagContentText "td"
  let mWeight = Just . fromMaybe 200 . tReadMaybe =<< mWeightT
  mBirthDate <- tagContentText "td"
  mBirthCity <- tagContentText "td"
  mNationality <- tagContentText "td"
  mHighSchool <- tagContentText "td"
  pure $ Player
     <$> mName
     <*> Just teamName
     <*> mPosition
     <*> (pure 0)
     <*> mHeight
     <*> mWeight
     <*> mNumber
     <*> mClass
     <*> mBirthDate
     <*> mBirthCity
     <*> mNationality
     <*> mHighSchool
  
downloadTeams :: MonadIO m => Manager -> m [Team]
downloadTeams manager = do
  res <- liftIO $ httpLbs "https://realgm.com/ncaa/teams" manager
  let tkns = either (const []) parseTokens . decodeUtf8' . LBS.toStrict $ responseBody res
      getTeams = getTeam >>= \case
        Nothing -> pure []
        Just team -> (team:) <$> getTeams
  evalStateT getTeams tkns

getTeam :: Monad m => Html m (Maybe Team)
getTeam = do
  tr
  tdClassNoWrap
  mTeamUrl <- fmap ("https://basketball.realgm.com" <>) <$> aHref
  mTeamName <- contentText
  pure $ Team <$> mTeamUrl <*> mTeamName <*> pure "" <*> pure 0.0

downloadNewData
  :: ( -- MonadReader Connection m
     -- , MonadError QueryError m
       MonadIO m
     -- , MonadIO m
     , MonadBaseControl IO m
     , MonadFail m
     )
  => m (Map Text TeamObject)
downloadNewData = do
  manager <- liftIO $ newManager tlsManagerSettings
  teams' <- downloadTeams manager
  -- let teams = dropWhile ((/= "Florida") . _team_name) teams'
  -- traverse_ Teams.create teams
  fmap M.fromList . for teams' $ \team -> do
    liftIO . putStrLn $ "Downloading team " ++ T.unpack (team ^. team_name) 
    players <- downloadPlayers manager team
    liftIO $ threadDelay 1000000
    liftIO . putStrLn $ "Finished downloading players for " ++ T.unpack (team ^. team_name) 
    ~(teamImage, matchups) <- downloadMatchupsAndPerformances manager team
    liftIO . putStrLn $ "Finished downloading matchups and performances for " ++ T.unpack (team ^. team_name) 
    -- Teams.updateImage (team ^. team_name) teamImage
    -- traverse_ Players.create players
    -- for_ matchups $ \(matchup, performances) -> do
    --   void $ Matchups.create matchup
    --   traverse_ Performances.create performances
    let matchupMap = O.fromList $ (\m -> (m ^. matchup_id, m)) . fst <$> matchups
        playerMap = M.fromList $ (\p -> (p ^. player_name, p)) <$> players
        perfMap = M.fromList $ (\p -> ((p ^. performance_matchupId, p ^. performance_player), p)) <$> concat (snd <$> matchups)
        teamObj = TeamObject (team & team_image .~ teamImage) matchupMap playerMap perfMap
    liftIO $ LBS.appendFile "/Users/dustinnorwood/Teams.txt" $ Aeson.encode teamObj
    liftIO $ SIO.appendFile "/Users/dustinnorwood/Teams.txt" "\n"
    pure (team ^. team_name, teamObj)