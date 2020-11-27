module Sudoku.CandidateAnnotations
  ( CandidateAnnotations
  , fromPartialColoring
  ) where

import Prelude

import Data.Map as Map
import Data.Set as Set

import Sudoku.PartialColoring (PartialColoring, Coord)

newtype CandidateAnnotations = CandidateAnnotations (Map.Map Coord (Set.Set Coord))

fromPartialColoring :: PartialColoring -> CandidateAnnotations
fromPartialColoring coloring = CandidateAnnotations Map.empty
