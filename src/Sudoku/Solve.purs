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

-- Cross-Hatching is identifying any uncolored vertex within a 9-clique that
-- can uniquely be assigned a particular color. It is equivalent to the "Hidden
-- Single" strategy, but using only the trivial annotations (annotations that
-- do no violate the rules of the game).
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
