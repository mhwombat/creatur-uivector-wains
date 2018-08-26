------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Util
-- Copyright   :  (c) Amy de Buitléir 2011-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.UIVector.Util
  (
    formatVector
  ) where

import Data.List (intercalate)
import Text.Printf (printf)

formatVector :: String -> [Double] -> String
formatVector fmt = intercalate " " . map (printf fmt)
