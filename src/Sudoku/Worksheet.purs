module Sudoku.Worksheet
  ( Worksheet(..)
  , from2dArray
  , setVertexColor
  ) where

import Prelude

import Sudoku.VertexColor (VertexColor)
import Sudoku.PartialColoring (PartialColoring, Coord)
import Sudoku.PartialColoring as PC

newtype Worksheet = Worksheet PartialColoring

from2dArray :: Array (Array Int) -> Worksheet
from2dArray = Worksheet <<< PC.from2dArray

setVertexColor :: Coord -> VertexColor -> Worksheet -> Worksheet
setVertexColor coord color (Worksheet coloring) = Worksheet <<< PC.setVertexColor coord color $ coloring
