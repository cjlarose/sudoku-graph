module Sudoku.CandidateAnnotations
  ( CandidateAnnotations
  , fromPartialColoring
  , candidatesForCoord
  ) where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe)

import Sudoku.PartialColoring (PartialColoring, Coord, adjacentVertices, uncoloredVerticies, getVertexColor)
import Sudoku.VertexColor (VertexColor, allColors)

newtype CandidateAnnotations = CandidateAnnotations (Map.Map Coord (Set.Set VertexColor))

fromPartialColoring :: PartialColoring -> CandidateAnnotations
fromPartialColoring coloring = CandidateAnnotations <<< Map.fromFoldable <<< Set.map (\v -> Tuple v $ candidates v) <<< uncoloredVerticies $ coloring
  where
    candidates :: Coord -> Set.Set VertexColor
    candidates = Set.difference allColors <<< Set.mapMaybe (\v -> getVertexColor v coloring) <<< adjacentVertices

candidatesForCoord :: Coord -> CandidateAnnotations -> Maybe (Set.Set VertexColor)
candidatesForCoord coord (CandidateAnnotations annotations) = Map.lookup coord annotations
