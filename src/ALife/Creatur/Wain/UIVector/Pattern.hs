------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.Pattern
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with patterns.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.UIVector.Pattern
  (
    Pattern,
    diff,
    makeSimilar
  ) where

import ALife.Creatur.Wain.UnitInterval (UIDouble, adjustUIVector)
import ALife.Creatur.Wain.Weights (Weights, weightedUIVectorDiff)

type Pattern = [UIDouble]

diff :: Weights -> Pattern -> Pattern -> UIDouble
diff = weightedUIVectorDiff

makeSimilar :: Pattern -> UIDouble -> Pattern -> Pattern
makeSimilar = adjustUIVector
