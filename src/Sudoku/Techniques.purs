module Sudoku.Techniques
  ( findCrossHatch
  , findNakedSingle
  , findHiddenSingle
  , findHiddenNTuple
  , findClaimingVerticies
  , findNakedNTuple
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Data.Array as Array
import Data.List.Lazy as LazyList
import Control.MonadZero (guard)
import Data.Foldable (and)

import Sudoku.VertexColor (VertexColor, allColors)
import Sudoku.PartialColoring (Coord, cliques, uncoloredVerticies)
import Sudoku.CandidateAnnotations (CandidateAnnotations, candidatesForCoord)
import Sudoku.CandidateAnnotations as CA
import Sudoku.Worksheet (Worksheet, AnnotatedWorksheet(..), addAnnotations)
import Sudoku.Suggestion (SuggestedAction(..))
import Sudoku.SetUtils (kCombinations, catMaybes)

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
findHiddenSingle (AnnotatedWorksheet ws) = LazyList.head do
  clique <- LazyList.fromFoldable cliques
  color <- LazyList.fromFoldable allColors
  let verticies = vertexSubsetWithCandidate color ws.annotations clique
  guard $ Set.size verticies == 1
  coord <- LazyList.fromFoldable <<< Set.findMin $ verticies
  pure $ FillCell { coord: coord, color: color }

 -- If in a house (9-clique), there exists a set of k verticies that contain as
 -- a subset each of the vertex candidate sets for k colors, then those
 -- verticies cannot be any other color.
findHiddenNTuple :: Int -> AnnotatedWorksheet -> Maybe SuggestedAction
findHiddenNTuple k (AnnotatedWorksheet { annotations: annotations, coloring: coloring }) = LazyList.head do
  clique <- LazyList.fromFoldable cliques
  let uncoloredInClique = Set.intersection clique <<< uncoloredVerticies $ coloring
  verticies <- LazyList.fromFoldable <<< kCombinations k $ uncoloredInClique
  colors <- LazyList.fromFoldable <<< kCombinations k <<< candidatesInVertexSet annotations $ uncoloredInClique
  guard <<< and <<< Set.map (\color -> Set.subset (vertexSubsetWithCandidate color annotations clique) verticies) $ colors
  let unionOfCandidates = candidatesInVertexSet annotations verticies
  let otherColors = Set.difference unionOfCandidates colors
  guard <<< not <<< Set.isEmpty $ otherColors
  pure $ RemoveCandidates { coords: Array.fromFoldable verticies, colors: otherColors }

candidatesInVertexSet :: CandidateAnnotations -> Set.Set Coord -> Set.Set VertexColor
candidatesInVertexSet annotations = Set.unions <<< catMaybes <<< Set.map (flip candidatesForCoord annotations)

vertexSubsetWithCandidate :: VertexColor -> CandidateAnnotations -> Set.Set Coord -> Set.Set Coord
vertexSubsetWithCandidate color annotations xs = Set.filter hasColorAsCandidate xs
  where
    hasColorAsCandidate :: Coord -> Boolean
    hasColorAsCandidate coord =
      case candidatesForCoord coord annotations of
        Nothing -> false
        Just candidates -> Set.member color candidates

-- Consider all pairs of distinct cliques with non-null intersections (c1, c2)
-- If all candidates for a color X in c1 are in the intersection, then we can
-- infer that X is not a candidate in c2 \ c1
-- Report only if there exists a cell with candidate X in c2 \ c1
-- Also if c1 is a row and c2 is a column, then the intersection is at most one cell
-- In this case, it makes more sense to solve with hidden single or naked single
-- So let's just consider pairs of cliques with intersections greater than 1 cell
findClaimingVerticies :: AnnotatedWorksheet -> Maybe SuggestedAction
findClaimingVerticies (AnnotatedWorksheet ws) = LazyList.head do
  leftHouse <- LazyList.fromFoldable cliques
  rightHouse <- LazyList.fromFoldable cliques
  guard $ leftHouse /= rightHouse
  let intersection = Set.intersection leftHouse rightHouse
  guard $ Set.size intersection > 1
  color <- LazyList.fromFoldable allColors
  let claimingVerticies = vertexSubsetWithCandidate color ws.annotations leftHouse
  guard <<< not <<< Set.isEmpty $ claimingVerticies
  guard $ claimingVerticies `Set.subset` intersection
  let cellsInRightHouse = vertexSubsetWithCandidate color ws.annotations rightHouse
  let cellsToRemoveCandidates = Set.difference cellsInRightHouse intersection
  guard <<< not <<< Set.isEmpty $ cellsToRemoveCandidates
  pure $ RemoveCandidates { coords: Set.toUnfoldable cellsToRemoveCandidates, colors: Set.singleton color }

-- If in any subset of the uncolored verticies in a 9-clique, the number of
-- distinct candidates among them is equal to the cardinality of the subset k,
-- then we can remove the candidates from the remaining verticies in the
-- clique
--
-- If k = 1, that's just a Naked Single, in which case we can fill a cell
-- immediately instead of just removing candidates.
--
-- For k in { 2, 3, 4 }, this technique is commonly called "Naked Pair",
-- "Naked Triplets", and "Naked Quads". Extending past k = 4 doesn't make
-- sense since if there exists a "Naked Quintuple", in the worst case the
-- remaning four cells in the clique form a Naked Quad
findNakedNTuple :: Int -> AnnotatedWorksheet -> Maybe SuggestedAction
findNakedNTuple k (AnnotatedWorksheet { coloring: coloring, annotations: annotations }) = LazyList.head do
  clique <- LazyList.fromFoldable cliques
  verticies <- LazyList.fromFoldable <<< kCombinations k <<< Set.intersection clique <<< uncoloredVerticies $ coloring
  let candidates = Set.unions <<< catMaybes <<< Set.map (flip candidatesForCoord annotations) $ verticies
  guard $ Set.size candidates == k
  let remainingVertices = Set.difference clique verticies
  let hasCandidatesToRemove = (\coord -> case candidatesForCoord coord annotations of
                                           Nothing -> false
                                           Just xs -> not <<< Set.isEmpty <<< Set.intersection candidates $ xs)
  let coordsToRemoveCandidates = Set.filter hasCandidatesToRemove remainingVertices
  guard <<< not <<< Set.isEmpty $ coordsToRemoveCandidates
  pure $ RemoveCandidates { coords: Set.toUnfoldable coordsToRemoveCandidates, colors: candidates }
