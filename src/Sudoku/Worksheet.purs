module Sudoku.Worksheet
  ( Worksheet(..)
  , AnnotatedWorksheet(..)
  , from2dArray
  , setVertexColor
  , showWorksheet
  , showAnnotatedWorksheet
  , addAnnotations
  , complete
  ) where

import Prelude

import Data.Set as Set

import Sudoku.VertexColor (VertexColor)
import Sudoku.Grid (fromPartialColoring, showGrid)
import Sudoku.PartialColoring (PartialColoring, Coord)
import Sudoku.PartialColoring as PC
import Sudoku.CandidateAnnotations (CandidateAnnotations)
import Sudoku.CandidateAnnotations as CA

newtype Worksheet = Worksheet PartialColoring
newtype AnnotatedWorksheet = AnnotatedWorksheet { coloring :: PartialColoring
                                                , annotations :: CandidateAnnotations }

from2dArray :: Array (Array Int) -> Worksheet
from2dArray = Worksheet <<< PC.from2dArray

setVertexColor :: Coord -> VertexColor -> Worksheet -> Worksheet
setVertexColor coord color (Worksheet coloring) = Worksheet <<< PC.setVertexColor coord color $ coloring

showWorksheet :: Worksheet -> String
showWorksheet (Worksheet coloring) = showGrid <<< fromPartialColoring $ coloring

showAnnotatedWorksheet :: AnnotatedWorksheet -> String
showAnnotatedWorksheet (AnnotatedWorksheet worksheet) = showGrid <<< fromPartialColoring $ worksheet.coloring

addAnnotations :: Worksheet -> AnnotatedWorksheet
addAnnotations (Worksheet coloring) = AnnotatedWorksheet { coloring: coloring
                                                         , annotations: CA.fromPartialColoring $ coloring }

complete :: Worksheet -> Boolean
complete (Worksheet coloring) = PC.complete coloring
