module Sudoku.CandidateAnnotations
  ( CandidateAnnotations
  , fromPartialColoring
  , candidatesForCoord
  , showCandidates
  , removeCandidates
  ) where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)

import Sudoku.PartialColoring (PartialColoring, Coord, adjacentVertices, uncoloredVerticies, getVertexColor)
import Sudoku.VertexColor (VertexColor, allColors, toInt)

newtype CandidateAnnotations = CandidateAnnotations (Map.Map Coord (Set.Set VertexColor))

fromPartialColoring :: PartialColoring -> CandidateAnnotations
fromPartialColoring coloring = CandidateAnnotations <<< Map.fromFoldable <<< Set.map (\v -> Tuple v $ candidates v) <<< uncoloredVerticies $ coloring
  where
    candidates :: Coord -> Set.Set VertexColor
    candidates = Set.difference allColors <<< Set.mapMaybe (\v -> getVertexColor v coloring) <<< adjacentVertices

candidatesForCoord :: Coord -> CandidateAnnotations -> Maybe (Set.Set VertexColor)
candidatesForCoord coord (CandidateAnnotations annotations) = Map.lookup coord annotations

showCandidates :: CandidateAnnotations -> String
showCandidates (CandidateAnnotations annotations) = joinWith "\n" <<< map showAnnotation <<< Map.toUnfoldable $ annotations
  where
    showColors :: Set.Set VertexColor -> String
    showColors = joinWith ", " <<< Set.toUnfoldable <<< Set.map (show <<< toInt)

    showAnnotation :: Tuple Coord (Set.Set VertexColor) -> String
    showAnnotation (Tuple (Tuple i j) candidates) = "candidates(" <> show i <> ", " <> show j <> ") = { " <> showColors candidates <> " } "

removeCandidates :: Coord -> VertexColor -> CandidateAnnotations -> CandidateAnnotations
removeCandidates coord color (CandidateAnnotations annotations) = CandidateAnnotations newAnnotations
  where
    neighbors :: Set.Set Coord
    neighbors = adjacentVertices coord

    updates :: Map.Map Coord (Set.Set VertexColor)
    updates = Map.mapMaybe (Just <<< Set.delete color) <<< Map.filterKeys (\v -> v `Set.member` neighbors) $ annotations

    newAnnotations :: Map.Map Coord (Set.Set VertexColor)
    newAnnotations = Map.delete coord <<< Map.union updates $ annotations
