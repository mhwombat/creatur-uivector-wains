------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.PatternQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.UIVector.PatternQC
  (
    test
  ) where

import ALife.Creatur.Wain.Weights (Weights, makeWeights)
import ALife.Creatur.Wain.UIVector.Pattern
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiVectorDiff)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- instance Arbitrary UIDouble where
--   arbitrary = doubleToUI <$> choose unitInterval

sizedArbPattern :: Int -> Gen Pattern
sizedArbPattern n = vectorOf n arbitrary

-- instance Arbitrary Pattern where
--   arbitrary = sized sizedArbPattern

sizedArbWeights :: Int -> Gen Weights
sizedArbWeights n = fmap makeWeights $ vectorOf n arbitrary

instance Arbitrary Weights where
  arbitrary = sized sizedArbWeights

prop_diff_can_be_0 :: Weights -> Pattern -> Property
prop_diff_can_be_0 ws xs = property $ diff ws xs xs == 0

prop_diff_btw_0_and_1 :: Weights -> Pattern -> Pattern -> Property
prop_diff_btw_0_and_1 ws xs ys = property $ 0 <= z && z <= 1
  where z = diff ws xs ys

prop_diff_symmetric :: Weights -> Pattern -> Pattern -> Property
prop_diff_symmetric ws xs ys = property $
  diff ws xs ys == diff ws ys xs

data TwoPatternsSameLength = TwoPatternsSameLength Pattern Pattern 
  deriving Show

sizedTwoPatternsSameLength :: Int -> Gen TwoPatternsSameLength
sizedTwoPatternsSameLength n = 
  TwoPatternsSameLength <$> sizedArbPattern n <*> sizedArbPattern n

instance Arbitrary TwoPatternsSameLength where
  arbitrary = sized sizedTwoPatternsSameLength

prop_zero_adjustment_is_no_adjustment :: 
  Weights -> TwoPatternsSameLength -> Property
prop_zero_adjustment_is_no_adjustment ws (TwoPatternsSameLength a b) =
  property $ diff ws b b' < aTad
  where b' = makeSimilar a 0 b
        aTad = 1e-10

prop_full_adjustment_gives_perfect_match :: 
  Weights -> TwoPatternsSameLength -> Property
prop_full_adjustment_gives_perfect_match
  ws (TwoPatternsSameLength a b) = property $ diff ws b' a < aTad
  where b' = makeSimilar a 1 b
        aTad = 1e-10

prop_makeSimilar_improves_similarity :: 
  Weights -> TwoPatternsSameLength -> UIDouble -> Property
prop_makeSimilar_improves_similarity ws (TwoPatternsSameLength a b) r
  = not (null a) && a /= b ==> d2 < d1
      where d1 = diff ws a b
            d2 = diff ws a b'
            b' = makeSimilar a r b

equiv :: Pattern -> Pattern -> Bool
equiv a b = uiVectorDiff a b <= aTad -- use unweighted diff
  where aTad = 0.00001

test :: Test
test = testGroup "ALife.Creatur.Wain.UIVector.PatternQC"
  [
    testProperty "prop_serialize_round_trippable - Pattern"
      (prop_serialize_round_trippable :: Pattern -> Property),
    testProperty "prop_genetic_round_trippable - Pattern"
      (prop_genetic_round_trippable equiv :: Pattern -> Property),
    testProperty "prop_diploid_identity - Pattern"
      (prop_diploid_identity (==) :: Pattern -> Property),
    testProperty "prop_diff_can_be_0"
      prop_diff_can_be_0,
    testProperty "prop_diff_btw_0_and_1"
      prop_diff_btw_0_and_1,
    testProperty "prop_diff_symmetric"
      prop_diff_symmetric,
    testProperty "prop_zero_adjustment_is_no_adjustment"
      prop_zero_adjustment_is_no_adjustment,
    testProperty "prop_full_adjustment_gives_perfect_match"
      prop_full_adjustment_gives_perfect_match,
    testProperty "prop_makeSimilar_improves_similarity"
      prop_makeSimilar_improves_similarity
  ]
