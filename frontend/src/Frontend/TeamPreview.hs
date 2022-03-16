{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
module Frontend.TeamPreview where


import Reflex.Dom.Core

import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (bool)
import           Data.Functor           (void)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)

import Common.Route
import qualified Frontend.Client        as Client
import Frontend.Utils (imgUrl, routeLinkDyn, routeLinkDynClass)

import           Common.Api.Teams.Team

teamsPreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , MonadSample t m
     , Prerender t m
     )
  => Dynamic t (Map Text Team, Maybe Text)
  -> m ()
teamsPreview artMapDyn = dyn_ $ loaded <$> artMapDyn
  where
    loaded (m, b) = elClass "div" "teams-preview" $
        if Map.null m
        then blank
        else void $ list ((\(m, b) -> Map.mapWithKey (\k v -> (k, v, b)) m) <$> artMapDyn) $ teamPreview

teamPreviewSmall
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadSample t m
     )
  => Dynamic t (Text, Team) -> m ()
teamPreviewSmall teamDyn = elClass "div" "team-preview" $ do
  elClass "div" "team-meta" $ do
    -- teamImage "thumbnail" $ teamModelImage . snd <$> teamDyn
    routeLinkDynClass (constDyn "preview-link")
      ((\a -> FrontendRoute_Team :/ (TeamName $ fst a)) <$> teamDyn)
      $ do
        dynText $ _team_name . snd <$> teamDyn

teamPreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     , MonadSample t m
     , Prerender t m
     )
  => Dynamic t (Text, Team, Maybe Text) -> m ()
teamPreview teamUserDyn' = do
  let teamUserDyn = teamUserDyn' -- <- fmap join $ foldDyn (\pm d -> fmap (\(s,_,mt) -> (s,pm,mt)) d) teamUserDyn' teamE
  let teamD = (\(s,pm,_) -> (s,pm)) <$> teamUserDyn
  routeLinkDynClass (constDyn "team-preview")
    ((\a -> FrontendRoute_Team :/ (TeamName $ fst a)) <$> teamD)
    $ do
    -- elClass "div" "team-meta" $ mdo
        -- didIAlreadyFavoriteIt = do
        --   (_, TeamModel{..}, mUser) <- teamUserDyn
        --   pure $ ffor mUser $ \t -> t `S.member` teamModelFavorited
        -- bClass = ffor didIAlreadyFavoriteIt $ \case
        --   Nothing -> "disabled "
        --   Just True -> "selected "
        --   Just False -> ""
    -- teamImage "team-preview-image" $ teamModelImage . snd <$> teamD
    -- let dAttrs = (\b -> ("class" =: (b <> "btn btn-outline-primary btn-sm pull-xs-right"))) <$> bClass
    -- (r,_) <- elDynAttr' "button" dAttrs $ do
    --   elClass "i" "favorite" blank
    --   text " "
    --   dynText $ T.pack . show . S.size . teamModelFavorited . snd <$> teamD
    -- let favoriteE = tag ((,) <$> (fst <$> current teamD) <*> (current didIAlreadyFavoriteIt)) $ domEvent Click r
    -- let urlE = ffor favoriteE $ \(s, b) -> case b of
    --       Just True -> Just $ BackendRoute_Api :/ ApiRoute_Team :/ (TeamName s, Just (TeamRoute_Unfavorite :/ ()))
    --       Just False -> Just $ BackendRoute_Api :/ ApiRoute_Team :/ (TeamName s, Just (TeamRoute_Favorite :/ ()))
    --       Nothing -> Nothing
    -- mTeamE <- Client.backendPostEvent ((,()) <$> fmapMaybe id urlE)
    -- let teamE = fmapMaybe id mTeamE
    elClass "div" "team-preview-title" $ el "h3" $ dynText $ _team_name . snd <$> teamD
    -- elClass "div" "team-preview-description" $ el "p" $ dynText $ (<>"...") . T.unwords . take 10 . T.words . teamModelDescription . snd <$> teamD
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
    ((\i -> Map.fromList [("src",imgUrl $ Just i),("class",className)]) <$> imageDyn)
    blank