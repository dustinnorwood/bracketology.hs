{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.HomePage where

import Reflex.Dom.Core

import           Common.Route
import           Control.Monad.Fix      (MonadFix)
import           Frontend.Matchup       (bracketPage)
import           Obelisk.Route.Frontend

homePage
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
homePage = bracketPage