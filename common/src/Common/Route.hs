{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Route where

import           Prelude hiding (id, (.))
import           Control.Category
import           Control.Lens hiding (bimap)
import           Control.Monad (join)
import           Data.Dependent.Sum (DSum)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Common.Api.Teams
import           Obelisk.Route
import           Obelisk.Route.TH

newtype TeamName = TeamName { unTeamName :: Text } deriving (Eq, Ord, Show)
makeWrapped ''TeamName
newtype PlayerName = PlayerName { unPlayerName :: Text } deriving (Eq, Ord, Show)
makeWrapped ''PlayerName
newtype Username = Username { unUsername :: Text } deriving (Eq, Ord, Show)
makeWrapped ''Username

data SearchTeamsParams = SearchTeamsParams
  { _search      :: Text
  } deriving (Eq, Ord, Show)
makeLenses ''SearchTeamsParams
type SearchTeams  = Windowed SearchTeamsParams
type GetTeamsFeed = Windowed ()

class RDefault a where
  rdef :: a

instance RDefault SearchTeamsParams where
  rdef = SearchTeamsParams ""

instance RDefault a => RDefault (Windowed a) where
  rdef = Windowed Nothing Nothing rdef

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (R ApiRoute)

data ApiRoute :: * -> * where
  ApiRoute_Teams :: ApiRoute (R TeamsRoute)
  ApiRoute_Team  :: ApiRoute (R TeamRoute)
  ApiRoute_Matchup :: ApiRoute (R MatchupRoute)
  ApiRoute_Player  :: ApiRoute (TeamName, PlayerName)
  ApiRoute_Bracket :: ApiRoute ()

tshow :: Show a => a -> Text
tshow = T.pack . show

queryEncoder :: (Applicative check, Applicative parse, Read a, Show a) => Encoder check parse (Maybe a) Text
queryEncoder = unsafeMkEncoder $ EncoderImpl
 (pure . readMaybe . T.unpack)
 (tshow)

mkQueryList :: [(Text, Maybe Text)] -> Map Text (Maybe Text)
mkQueryList = M.fromList . catMaybes . map (\(t, m) -> (t,) . Just <$> m)

windowedEncoder :: Encoder Identity Identity           decoded  (Map Text (Maybe Text))
                -> Encoder Identity Identity (Windowed decoded) (Map Text (Maybe Text))
windowedEncoder itemEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> Identity $ Windowed (decode queryEncoder . fromMaybe "" . join $ M.lookup "limit" m)
                             (decode queryEncoder . fromMaybe "" . join $ M.lookup "offset" m)
                             (decode itemEncoder m))
  (\(Windowed l o i) -> M.union (mkQueryList [("limit", tshow <$> l), ("offset", tshow <$> o)]) (encode itemEncoder i))

emptyGetTeams :: GetTeams
emptyGetTeams = Windowed Nothing Nothing ""

emptySearchTeams :: SearchTeams
emptySearchTeams = Windowed Nothing Nothing (SearchTeamsParams "")

homeRoute :: DSum FrontendRoute Identity
homeRoute = FrontendRoute_Home :/ emptyGetTeams

getTeamsParamsEncoder :: (Applicative check, Applicative parse) => Encoder check parse Text (Map Text (Maybe Text))
getTeamsParamsEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> pure $ fromMaybe "" $ join $ M.lookup "name" m)
  (\n -> mkQueryList [("name", Just n)])

getTeamsEncoder :: Encoder Identity Identity GetTeams (Map Text (Maybe Text))
getTeamsEncoder = windowedEncoder getTeamsParamsEncoder

searchTeamsParamsEncoder :: (Applicative check, Applicative parse) => Encoder check parse SearchTeamsParams (Map Text (Maybe Text))
searchTeamsParamsEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> pure $ SearchTeamsParams (fromMaybe "" . join $ M.lookup "search" m))
  (\(SearchTeamsParams s) -> mkQueryList [("search", Just s)])

searchTeamsEncoder :: Encoder Identity Identity SearchTeams (Map Text (Maybe Text))
searchTeamsEncoder = windowedEncoder searchTeamsParamsEncoder

getTeamsFeedEncoder :: Encoder Identity Identity GetTeamsFeed (Map Text (Maybe Text))
getTeamsFeedEncoder = windowedEncoder . unsafeMkEncoder $ EncoderImpl (pure . const ()) (const M.empty)

getTeamEncoder :: (Applicative check, Applicative parse) => Encoder check parse TeamName (Map Text (Maybe Text))
getTeamEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> pure $ TeamName (fromMaybe "" . join $ M.lookup "team-name" m))
  (\(TeamName s) -> mkQueryList [("team-name", Just s)])

data TeamsRoute :: * -> * where
  TeamsRoute_Get    :: TeamsRoute GetTeams
  TeamsRoute_Search :: TeamsRoute SearchTeams

data TeamRoute :: * -> * where
  TeamRoute_Get          :: TeamRoute TeamName
  TeamRoute_GetTeamNames :: TeamRoute ()

data MatchupRoute :: * -> * where
  MatchupRoute_Run :: MatchupRoute [Text]

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute GetTeams
  FrontendRoute_Search :: FrontendRoute SearchTeams
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_Team :: FrontendRoute TeamName
  FrontendRoute_Player :: FrontendRoute (TeamName, PlayerName)
  FrontendRoute_Matchup :: FrontendRoute (TeamName, TeamName)
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data ProfileRoute :: * -> * where
  ProfileRoute_Favourites :: ProfileRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing           -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api               -> PathSegment "api" $ apiRouteEncoder
  )
  (\case
      FrontendRoute_Home -> PathEnd
                          . hoistParse (pure . runIdentity)
                          $ hoistCheck (pure . runIdentity) getTeamsEncoder
      FrontendRoute_Search -> PathSegment "search"
                            $ queryOnlyEncoder .
                            ( hoistParse (pure . runIdentity)
                            . hoistCheck (pure . runIdentity)
                            $ searchTeamsEncoder
                            )
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
      FrontendRoute_Team -> PathSegment "team" $ singlePathSegmentEncoder . unwrappedEncoder
      FrontendRoute_Player -> PathSegment "player" $ pathParamEncoder unwrappedEncoder $ singlePathSegmentEncoder . unwrappedEncoder
      FrontendRoute_Matchup -> PathSegment "matchup" $ pathParamEncoder unwrappedEncoder $ singlePathSegmentEncoder . unwrappedEncoder
  )

apiRouteEncoder :: Encoder (Either Text) (Either Text) (R ApiRoute) PageName
apiRouteEncoder = pathComponentEncoder $ \case
  ApiRoute_Teams -> PathSegment "teams" $ teamsRouteEncoder
  ApiRoute_Team -> PathSegment "team" $ teamRouteEncoder
  ApiRoute_Matchup -> PathSegment "matchup" $ matchupRouteEncoder
  ApiRoute_Player -> PathSegment "player" $ pathParamEncoder unwrappedEncoder $ singlePathSegmentEncoder . unwrappedEncoder
  ApiRoute_Bracket -> PathEnd $ unitEncoder mempty

teamsRouteEncoder :: Encoder (Either Text) (Either Text) (R TeamsRoute) PageName
teamsRouteEncoder = pathComponentEncoder $ \case
  TeamsRoute_Get -> PathEnd
                  . hoistParse (pure . runIdentity)
                  $ hoistCheck (pure . runIdentity) getTeamsEncoder
  TeamsRoute_Search -> PathSegment "search"
                     $ queryOnlyEncoder .
                     ( hoistParse (pure . runIdentity)
                     . hoistCheck (pure . runIdentity)
                     $ searchTeamsEncoder
                     )

teamRouteEncoder :: Encoder (Either Text) (Either Text) (R TeamRoute) PageName
teamRouteEncoder = pathComponentEncoder $ \case
  TeamRoute_Get          -> PathSegment "get" $ singlePathSegmentEncoder . unwrappedEncoder
  TeamRoute_GetTeamNames -> PathSegment "all" $ unitEncoder mempty

matchupRouteEncoder :: Encoder (Either Text) (Either Text) (R MatchupRoute) PageName
matchupRouteEncoder = pathComponentEncoder $ \case
  MatchupRoute_Run -> PathSegment "run" $ pathOnlyEncoder

checkedEncoder :: Applicative check => Encoder check Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedEncoder = either (error "checkEncoder failed") id $ checkEncoder fullRouteEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''ApiRoute
  , ''TeamsRoute
  , ''TeamRoute
  , ''MatchupRoute
  , ''FrontendRoute
  ]
