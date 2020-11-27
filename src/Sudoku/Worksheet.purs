module Sudoku.Worksheet
  ( Worksheet(..)
  , AnnotatedWorksheet(..)
  , from2dArray
  , setVertexColor
  , setVertexColorWithAnnotations
  , showWorksheet
  , showAnnotatedWorksheet
  , addAnnotations
  , complete
  , completeWithAnnotations
  ) where

import Prelude

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

setVertexColorWithAnnotations :: Coord -> VertexColor -> AnnotatedWorksheet -> AnnotatedWorksheet
setVertexColorWithAnnotations coord color (AnnotatedWorksheet ws) = AnnotatedWorksheet { coloring: newColoring, annotations: newAnnotations }
  where
    newColoring = PC.setVertexColor coord color ws.coloring
    newAnnotations = CA.removeCandidates coord color ws.annotations

showWorksheet :: Worksheet -> String
showWorksheet (Worksheet coloring) = showGrid <<< fromPartialColoring $ coloring

showAnnotatedWorksheet :: AnnotatedWorksheet -> String
showAnnotatedWorksheet (AnnotatedWorksheet worksheet) = coloringString <> "\n" <> candidateString
  where
    coloringString = showGrid <<< fromPartialColoring $ worksheet.coloring
    candidateString = CA.showCandidates worksheet.annotations

addAnnotations :: Worksheet -> AnnotatedWorksheet
addAnnotations (Worksheet coloring) = AnnotatedWorksheet { coloring: coloring
                                                         , annotations: CA.fromPartialColoring $ coloring }

complete :: Worksheet -> Boolean
complete (Worksheet coloring) = PC.complete coloring

completeWithAnnotations :: AnnotatedWorksheet -> Boolean
completeWithAnnotations (AnnotatedWorksheet ws) = PC.complete ws.coloring
