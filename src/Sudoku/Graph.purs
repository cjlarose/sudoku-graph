module Sudoku.Graph
  ( toGraph
  , rowCoords
  , colCoords
  ) where

import Prelude

import Data.Array ((!!), (..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)

import Sudoku.VertexColor (VertexColor)
import Sudoku.Grid (Grid)

type Coord = Tuple Int Int
type Graph = Map.Map Coord VertexColor

fromPairs :: Array (Tuple Coord (Maybe VertexColor)) -> Graph
fromPairs = foldl addVertexIfColored Map.empty
  where
    addVertexIfColored :: Graph -> Tuple Coord (Maybe VertexColor) -> Graph
    addVertexIfColored acc (Tuple coord (Just color)) = Map.insert coord color acc
    addVertexIfColored acc (Tuple coord _) = acc

toGraph :: Grid -> Graph
toGraph grid = fromPairs pairsWithColors
  where
    vertexColorAtCoord :: Coord -> Maybe VertexColor
    vertexColorAtCoord (Tuple i j) = do
      row <- grid !! i
      join (row !! j)

    pairsWithColors :: Array (Tuple Coord (Maybe VertexColor))
    pairsWithColors = do
      i <- 0 .. 8
      j <- 0 .. 8
      let coord = Tuple i j
      pure $ Tuple coord (vertexColorAtCoord coord)

rowCoords :: Array (Set.Set Coord)
rowCoords = map coordsForRow $ 0 .. 8
  where
    coordsForRow :: Int -> Set.Set Coord
    coordsForRow i = Set.fromFoldable <<< map (Tuple i) $ 0 .. 8

colCoords :: Array (Set.Set Coord)
colCoords = map coordsForCol $ 0 .. 8
  where
    coordsForCol :: Int -> Set.Set Coord
    coordsForCol j = Set.fromFoldable <<< map (\i -> Tuple i j) $ 0 .. 8
