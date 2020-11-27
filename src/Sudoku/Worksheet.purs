module Sudoku.Worksheet
  ( Worksheet(..)
  , from2dArray
  , setVertexColor
  , showWorksheet
  ) where

import Prelude

import Sudoku.VertexColor (VertexColor)
import Sudoku.Grid (fromPartialColoring, showGrid)
import Sudoku.PartialColoring (PartialColoring, Coord)
import Sudoku.PartialColoring as PC

newtype Worksheet = Worksheet PartialColoring

from2dArray :: Array (Array Int) -> Worksheet
from2dArray = Worksheet <<< PC.from2dArray

setVertexColor :: Coord -> VertexColor -> Worksheet -> Worksheet
setVertexColor coord color (Worksheet coloring) = Worksheet <<< PC.setVertexColor coord color $ coloring

showWorksheet :: Worksheet -> String
showWorksheet (Worksheet coloring) = showGrid <<< fromPartialColoring $ coloring
