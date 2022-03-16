{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Logging where

import           Control.Monad.FT.Put
import           Data.Functor.Identity
import           Data.Text                   as T
import qualified Data.Text                   as T

newtype LogMessage = LogMessage { unLogMessage :: Text }

instance Puttable LogMessage Identity where
  put _ = pure ()