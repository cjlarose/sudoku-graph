module Sudoku.PartialColoring
  ( PartialColoring(..)
  , Coord
  , from2dArray
  , rowCoords
  , colCoords
  , blockCoords
  , cliques
  , adjacentVertices
  , uncoloredVerticies
  , getVertexColor
  , setVertexColor
  , complete
  , allCoords
  ) where

import Prelude

import Data.Array ((!!), (..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)

import Sudoku.VertexColor (VertexColor, fromInt)

type Coord = Tuple Int Int
newtype PartialColoring = PartialColoring (Map.Map Coord VertexColor)

derive instance eqPartialColoring :: Eq PartialColoring

instance showPartialColoring :: Show PartialColoring where
  show (PartialColoring coloring) = "(PartialColoring " <> show coloring <> ")"

allCoords :: Set.Set Coord
allCoords = Set.fromFoldable $ do
  i <- 0 .. 8
  j <- 0 .. 8
  pure $ Tuple i j

empty :: PartialColoring
empty = PartialColoring Map.empty

from2dArray :: Array (Array Int) -> PartialColoring
from2dArray grid = foldl addVertexIfColored empty <<< Set.map (\c -> Tuple c $ vertexColorAtCoord c) $ allCoords
  where
    vertexColorAtCoord :: Coord -> Maybe VertexColor
    vertexColorAtCoord (Tuple i j) = do
      row <- grid !! i
      val <- row !! j
      fromInt val

    addVertexIfColored :: PartialColoring -> Tuple Coord (Maybe VertexColor) -> PartialColoring
    addVertexIfColored acc (Tuple coord (Just color)) = setVertexColor coord color acc
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

blockCoords :: Array (Set.Set Coord)
blockCoords = map coordsForBlock $ do
  i <- 0 .. 2
  j <- 0 .. 2
  pure $ Tuple i j

  where
    coordsForBlock :: Tuple Int Int -> Set.Set Coord
    coordsForBlock (Tuple i j) = Set.fromFoldable $ do
      let startI = i * 3
      let startJ = j * 3
      i2 <- startI .. (startI + 2)
      j2 <- startJ .. (startJ + 2)
      pure $ Tuple i2 j2

-- | Returns the 27 9-cliques of the sudoku graph
cliques :: Set.Set (Set.Set Coord)
cliques = Set.unions <<< map Set.fromFoldable $ [rowCoords, colCoords, blockCoords]

adjacentVertices :: Coord -> Set.Set Coord
adjacentVertices coord = Set.delete coord <<< Set.unions <<< Set.filter (Set.member coord) $ cliques

uncoloredVerticies :: PartialColoring -> Set.Set Coord
uncoloredVerticies (PartialColoring coloring) = Set.difference allCoords <<< Map.keys $ coloring

getVertexColor :: Coord -> PartialColoring -> Maybe VertexColor
getVertexColor coord (PartialColoring coloring) = Map.lookup coord coloring

setVertexColor :: Coord -> VertexColor -> PartialColoring -> PartialColoring
setVertexColor coord color (PartialColoring coloring) = PartialColoring <<< Map.insert coord color $ coloring

complete :: PartialColoring -> Boolean
complete (PartialColoring coloring) = Set.isEmpty <<< Set.difference allCoords <<< Map.keys $ coloring
