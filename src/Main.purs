module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Array ((!!), (..))

import Sudoku.VertexColor (VertexColor)
import Sudoku.Grid (fromIntGrid, showGrid, Grid)

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

main :: Effect Unit
main = do
  let grid = fromIntGrid [[6, 8, 5, 0, 3, 0, 2, 9, 4]
                         ,[0, 0, 0, 0, 9, 2, 0, 0, 5]
                         ,[0, 0, 3, 0, 5, 6, 0, 7, 0]
                         ,[2, 1, 9, 6, 8, 3, 5, 4, 0]
                         ,[4, 5, 0, 9, 2, 0, 0, 8, 0]
                         ,[0, 0, 8, 0, 0, 5, 1, 2, 0]
                         ,[0, 0, 0, 0, 0, 9, 0, 1, 0]
                         ,[1, 9, 0, 0, 7, 0, 4, 6, 0]
                         ,[8, 7, 6, 3, 1, 0, 9, 0, 0]]
  log <<< showGrid $ grid
  let graph = toGraph grid
  logShow graph
