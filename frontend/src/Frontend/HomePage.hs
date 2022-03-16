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

import           Control.Monad.Fix      (MonadFix)
import           Frontend.Matchup       (bracketPage)

homePage
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     )
  => m ()
homePage = bracketPage