module Sudoku.VertexColor
  ( VertexColor
  , allColors
  ) where

import Prelude

import Data.Set as Set
import Data.Array ((..))

type VertexColor = Int

allColors :: Set.Set VertexColor
allColors = Set.fromFoldable $ 0 .. 8
