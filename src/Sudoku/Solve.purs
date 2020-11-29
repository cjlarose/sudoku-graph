module Sudoku.Solve
  ( findCrossHatch
  , findNakedSingle
  , findHiddenSingle
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Data.Array as Array
import Data.Array (findMap)

import Sudoku.VertexColor (VertexColor, allColors)
import Sudoku.PartialColoring (Coord, cliques, uncoloredVerticies)
import Sudoku.CandidateAnnotations (candidatesForCoord)
import Sudoku.CandidateAnnotations as CA
import Sudoku.Worksheet (Worksheet, AnnotatedWorksheet(..), addAnnotations)
import Sudoku.Suggestion (SuggestedAction(..))

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
findCrossHatch :: Worksheet -> Maybe SuggestedAction
findCrossHatch = findHiddenSingle <<< addAnnotations

-- A naked single is any candidate set of cardinality 1
-- Since only one candidate can go in that position, we fill that position with
-- the sole candidate
findNakedSingle :: AnnotatedWorksheet -> Maybe SuggestedAction
findNakedSingle (AnnotatedWorksheet ws) = do
   Tuple coord colors <- CA.find ((==) 1 <<< Set.size) ws.annotations
   color <- Set.findMin colors
   pure $ FillCell { coord: coord, color: color }

-- If a candidate color appears only once within any house (9-clique), that
-- cell must be colored with that candidate color
findHiddenSingle :: AnnotatedWorksheet -> Maybe SuggestedAction
findHiddenSingle (AnnotatedWorksheet ws) = findMap findColoringInClique <<< Array.fromFoldable $ cliques
  where
    hasColorAsCandidate :: VertexColor -> Coord -> Boolean
    hasColorAsCandidate color coord = case candidatesForCoord coord ws.annotations of
                                        Just colors -> Set.member color colors
                                        Nothing -> false

    findColoringInClique :: Set.Set Coord -> Maybe SuggestedAction
    findColoringInClique clique = findMap findSuggestion allColors
      where
        uncoloredInClique = Set.intersection clique $ uncoloredVerticies ws.coloring

        findSuggestion :: VertexColor -> Maybe SuggestedAction
        findSuggestion color = case Array.fromFoldable <<< Set.filter (hasColorAsCandidate color) $ uncoloredInClique of
                                 [coord] -> Just $ FillCell { coord: coord, color: color }
                                 _ -> Nothing
