module Sudoku.Suggestion
  ( Suggestion
  , SuggestedAction(..)
  , showSuggestion
  ) where

import Prelude
import Data.Tuple (Tuple(..))

import Sudoku.VertexColor as Color
import Sudoku.PartialColoring (Coord)
import Sudoku.VertexColor (VertexColor)

newtype SuggestedAction = FillCell { coord :: Coord, color :: VertexColor }
type Suggestion = { strategyName :: String , action :: SuggestedAction }

showSuggestion :: Suggestion -> String
showSuggestion { strategyName: name, action: FillCell { coord: (Tuple i j), color: color } } =
  "Suggestion (" <> name <> "): Fill cell (" <> show i <> "," <> show j <> ")" <> " with value " <> show (Color.toInt color)

