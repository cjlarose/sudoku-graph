module Sudoku.Graph
  ( Graph
  , Coord
  , from2dArray
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

type Coord = Tuple Int Int
type Graph = Map.Map Coord VertexColor

allCoords :: Set.Set Coord
allCoords = Set.fromFoldable $ do
  i <- 0 .. 8
  j <- 0 .. 8
  pure $ Tuple i j

from2dArray :: Array (Array Int) -> Graph
from2dArray grid = foldl addVertexIfColored Map.empty <<< Set.map (\c -> Tuple c $ vertexColorAtCoord c) $ allCoords
  where
    vertexColorAtCoord :: Coord -> Maybe VertexColor
    vertexColorAtCoord (Tuple i j) = do
      row <- grid !! i
      val <- row !! j
      if val == 0 then Nothing else Just val

    addVertexIfColored :: Graph -> Tuple Coord (Maybe VertexColor) -> Graph
    addVertexIfColored acc (Tuple coord (Just color)) = Map.insert coord color acc
    addVertexIfColored acc (Tuple _ Nothing) = acc

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
