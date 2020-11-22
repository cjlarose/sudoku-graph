module Sudoku.VertexColor
  ( VertexColor(..)
  , allColors
  ) where

import Prelude

import Data.Set as Set
import Data.Array ((..))

newtype VertexColor = VertexColor Int

derive instance eqVertexColor :: Eq VertexColor
derive instance ordVertexColor :: Ord VertexColor

instance showVertexColor :: Show VertexColor where
  show (VertexColor color) = "(VertexColor " <> show color <> ")"

allColors :: Set.Set VertexColor
allColors = Set.map VertexColor <<< Set.fromFoldable $ 0 .. 8
