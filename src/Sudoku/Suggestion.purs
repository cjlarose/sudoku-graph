module Sudoku.Suggestion
  ( Suggestion
  , SuggestedAction(..)
  , showSuggestion
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.String (joinWith)

import Sudoku.VertexColor as Color
import Sudoku.PartialColoring (Coord)
import Sudoku.VertexColor (VertexColor)

data SuggestedAction = FillCell { coord :: Coord, color :: VertexColor }
                     | RemoveCandidates { coords :: Array Coord, color :: VertexColor }
type Suggestion = { strategyName :: String , action :: SuggestedAction }

derive instance eqSuggestedAction :: Eq SuggestedAction

instance showSuggestedAction :: Show SuggestedAction where
  show (FillCell r) = "(FillCell " <> show r <> ")"
  show (RemoveCandidates r) = "(RemoveCandidates " <> show r <> ")"

showSuggestion :: Suggestion -> String
showSuggestion { strategyName: name, action: action } = "Suggestion (" <> name <> "): " <> showAction
  where
    showCell (Tuple i j) = "(" <> show i <> "," <> show j <> ")"
    showCells = joinWith "," <<< map showCell
    showColor = show <<< Color.toInt
    showAction =
      case action of
        FillCell { coord: coord, color: color } -> "Fill cell " <> showCell coord <> " with value " <> showColor color
        RemoveCandidates { coords: coords, color: color } -> "Remove " <> showColor color <> " as a candidate from cells " <> showCells coords
