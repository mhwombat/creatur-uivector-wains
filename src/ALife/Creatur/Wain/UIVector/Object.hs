------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Object
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with objects that could be either wains or
-- records.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.UIVector.Object
  (
    Object(..),
    isPattern,
    objectId,
    objectNum,
    objectAppearance,
    objectEnergy,
    objectChildEnergy,
    addIfWain,
    objectToWain
  ) where

import ALife.Creatur (agentId)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.UIVector.Pattern (Pattern)
import ALife.Creatur.Wain.UIVector.Tweaker (PatternTweaker(..))
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Lens
import GHC.Generics (Generic)
import Data.Serialize

data Object a = PObject Pattern String
              | AObject (W.Wain Pattern PatternTweaker a)
              deriving (Eq, Show, Generic, Serialize)

isPattern :: Object a -> Bool
isPattern (PObject _ _) = True
isPattern (AObject _) = False

objectId :: Object a -> String
objectId (PObject _ s) = "UIVector " ++ s
objectId (AObject a) = agentId a

objectNum :: Object a -> Int
objectNum (PObject _ s) = read . take 1 . drop 1 $ s
objectNum (AObject _) = 10

objectAppearance :: Object a -> Pattern
objectAppearance (PObject img _) = img
objectAppearance (AObject a) = view W.appearance a

objectEnergy :: Object a -> UIDouble
objectEnergy (PObject _ _) = 0
objectEnergy (AObject a) = view W.energy a

objectChildEnergy :: Object a -> Double
objectChildEnergy (PObject _ _) = 0
objectChildEnergy (AObject a) = W.childEnergy a

addIfWain
  :: Object a -> [W.Wain Pattern PatternTweaker a]
    -> [W.Wain Pattern PatternTweaker a]
addIfWain (PObject _ _) xs = xs
addIfWain (AObject a) xs = a:xs

objectToWain :: Object a -> W.Wain Pattern PatternTweaker a
objectToWain (PObject _ _) = error "record, not wain"
objectToWain (AObject a) = a
