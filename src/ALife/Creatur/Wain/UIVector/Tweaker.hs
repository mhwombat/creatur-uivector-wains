------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Tweaker
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for comparing and adjusting vectors.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module ALife.Creatur.Wain.UIVector.Tweaker
  (
    PatternTweaker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Tweaker(..))
import qualified ALife.Creatur.Wain.UIVector.Pattern as P
import  ALife.Creatur.Wain.Pretty (Pretty)
import  ALife.Creatur.Wain.Statistics (Statistical(..), prefix)
import ALife.Creatur.Wain.Weights (Weights)

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data PatternTweaker = PatternTweaker Weights
  deriving (Eq, Show, Pretty, Generic)

instance Tweaker PatternTweaker where
  type Pattern PatternTweaker = P.Pattern
  diff (PatternTweaker ws) = P.diff ws
  adjust _ = P.makeSimilar

instance Serialize PatternTweaker
instance W8.Genetic PatternTweaker
instance Diploid PatternTweaker

instance Statistical PatternTweaker where
  stats (PatternTweaker ws) = map (prefix "pattern tweaker weights") . stats $ ws
