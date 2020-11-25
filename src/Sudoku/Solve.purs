module Sudoku.Solve
  ( tryCrossHatch
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Set as Set
import Data.Array as Array
import Data.Array (findMap)

import Sudoku.VertexColor (VertexColor, allColors)
import Sudoku.PartialColoring (PartialColoring, Coord, cliques, adjacentVertices, uncoloredVerticies, getVertexColor)

-- Typically, crosshatching is identifying, within a block, a unique cell where
-- a given candidate can be assigned cells within a block are eliminated if
-- they belong to a row or column that already has that candidate
--
-- In the graph, we can examine the nine vertices that correspond to a block.
-- Eliminate all vertices that are adjacent to a vertex colored with the given
-- color. If there's only one vertex remaining, we can assign the color to that
-- vertex This is because in each k_9 subgraph, exactly one vertex must have
-- each color
--
-- More generally, identify any 9-clique in the graph and apply the same
-- logic.
-- candidates = for each uncolored vertex, compute candidate colors by
--              eliminating colors of adjacent vertices for each clique
--   uncolored = uncolored vertex subset
--   for each color
--     compute the subset of uncolored vertices that have the color as a candidate
--     if the size of that subset is 1, color that vertex
tryCrossHatch :: PartialColoring -> Maybe (Tuple Coord VertexColor)
tryCrossHatch coloring = findMap findColoringInClique <<< Array.fromFoldable $ cliques
  where
    candidates :: Coord -> Set.Set VertexColor
    candidates = Set.difference allColors <<< Set.mapMaybe (\v -> getVertexColor v coloring) <<< adjacentVertices

    uncoloredVertexCandidates :: Map.Map Coord (Set.Set VertexColor)
    uncoloredVertexCandidates = Map.fromFoldable <<< Set.map (\v -> Tuple v $ candidates v) $ uncoloredVerticies coloring

    findColoringInClique :: Set.Set Coord -> Maybe (Tuple Coord VertexColor)
    findColoringInClique clique = findMap findSuggestion allColors
      where
        uncoloredInClique = Set.intersection clique $ uncoloredVerticies coloring

        hasColorAsCandidate :: VertexColor -> Coord -> Boolean
        hasColorAsCandidate color coord = case Map.lookup coord uncoloredVertexCandidates of
                                            Just colors -> Set.member color colors
                                            Nothing -> false

        findSuggestion :: VertexColor -> Maybe (Tuple Coord VertexColor)
        findSuggestion color = case Array.fromFoldable <<< Set.filter (hasColorAsCandidate color) $ uncoloredInClique of
                                 [coord] -> Just $ Tuple coord color
                                 _ -> Nothing
