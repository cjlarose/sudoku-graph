module Test.Sudoku.Grid
  ( testGrid
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Sudoku.VertexColor (VertexColor(..))
import Sudoku.Grid (fromGraph)

testFromGraph :: Effect Unit
testFromGraph = do
  let graph = Map.fromFoldable [Tuple (Tuple 0 0) (VertexColor 6)
                               ,Tuple (Tuple 0 1) (VertexColor 8)
                               ,Tuple (Tuple 0 2) (VertexColor 5)
                               ,Tuple (Tuple 0 4) (VertexColor 3)
                               ,Tuple (Tuple 0 6) (VertexColor 2)
                               ,Tuple (Tuple 0 7) (VertexColor 9)
                               ,Tuple (Tuple 0 8) (VertexColor 4)
                               ,Tuple (Tuple 1 4) (VertexColor 9)
                               ,Tuple (Tuple 1 5) (VertexColor 2)
                               ,Tuple (Tuple 1 8) (VertexColor 5)
                               ,Tuple (Tuple 2 2) (VertexColor 3)
                               ,Tuple (Tuple 2 4) (VertexColor 5)
                               ,Tuple (Tuple 2 5) (VertexColor 6)
                               ,Tuple (Tuple 2 7) (VertexColor 7)
                               ,Tuple (Tuple 3 0) (VertexColor 2)
                               ,Tuple (Tuple 3 1) (VertexColor 1)
                               ,Tuple (Tuple 3 2) (VertexColor 9)
                               ,Tuple (Tuple 3 3) (VertexColor 6)
                               ,Tuple (Tuple 3 4) (VertexColor 8)
                               ,Tuple (Tuple 3 5) (VertexColor 3)
                               ,Tuple (Tuple 3 6) (VertexColor 5)
                               ,Tuple (Tuple 3 7) (VertexColor 4)
                               ,Tuple (Tuple 4 0) (VertexColor 4)
                               ,Tuple (Tuple 4 1) (VertexColor 5)
                               ,Tuple (Tuple 4 3) (VertexColor 9)
                               ,Tuple (Tuple 4 4) (VertexColor 2)
                               ,Tuple (Tuple 4 7) (VertexColor 8)
                               ,Tuple (Tuple 5 2) (VertexColor 8)
                               ,Tuple (Tuple 5 5) (VertexColor 5)
                               ,Tuple (Tuple 5 6) (VertexColor 1)
                               ,Tuple (Tuple 5 7) (VertexColor 2)
                               ,Tuple (Tuple 6 5) (VertexColor 9)
                               ,Tuple (Tuple 6 7) (VertexColor 1)
                               ,Tuple (Tuple 7 0) (VertexColor 1)
                               ,Tuple (Tuple 7 1) (VertexColor 9)
                               ,Tuple (Tuple 7 4) (VertexColor 7)
                               ,Tuple (Tuple 7 6) (VertexColor 4)
                               ,Tuple (Tuple 7 7) (VertexColor 6)
                               ,Tuple (Tuple 8 0) (VertexColor 8)
                               ,Tuple (Tuple 8 1) (VertexColor 7)
                               ,Tuple (Tuple 8 2) (VertexColor 6)
                               ,Tuple (Tuple 8 3) (VertexColor 3)
                               ,Tuple (Tuple 8 4) (VertexColor 1)
                               ,Tuple (Tuple 8 6) (VertexColor 9)]
  let expectedGrid = [[(Just (VertexColor 6)),(Just (VertexColor 8)),(Just (VertexColor 5)),Nothing,(Just (VertexColor 3)),Nothing,(Just (VertexColor 2)),(Just (VertexColor 9)),(Just (VertexColor 4))]
                     ,[Nothing,Nothing,Nothing,Nothing,(Just (VertexColor 9)),(Just (VertexColor 2)),Nothing,Nothing,(Just (VertexColor 5))]
                     ,[Nothing,Nothing,(Just (VertexColor 3)),Nothing,(Just (VertexColor 5)),(Just (VertexColor 6)),Nothing,(Just (VertexColor 7)),Nothing]
                     ,[(Just (VertexColor 2)),(Just (VertexColor 1)),(Just (VertexColor 9)),(Just (VertexColor 6)),(Just (VertexColor 8)),(Just (VertexColor 3)),(Just (VertexColor 5)),(Just (VertexColor 4)),Nothing]
                     ,[(Just (VertexColor 4)),(Just (VertexColor 5)),Nothing,(Just (VertexColor 9)),(Just (VertexColor 2)),Nothing,Nothing,(Just (VertexColor 8)),Nothing]
                     ,[Nothing,Nothing,(Just (VertexColor 8)),Nothing,Nothing,(Just (VertexColor 5)),(Just (VertexColor 1)),(Just (VertexColor 2)),Nothing]
                     ,[Nothing,Nothing,Nothing,Nothing,Nothing,(Just (VertexColor 9)),Nothing,(Just (VertexColor 1)),Nothing]
                     ,[(Just (VertexColor 1)),(Just (VertexColor 9)),Nothing,Nothing,(Just (VertexColor 7)),Nothing,(Just (VertexColor 4)),(Just (VertexColor 6)),Nothing]
                     ,[(Just (VertexColor 8)),(Just (VertexColor 7)),(Just (VertexColor 6)),(Just (VertexColor 3)),(Just (VertexColor 1)),Nothing,(Just (VertexColor 9)),Nothing,Nothing]]
  assertEqual { expected: expectedGrid, actual: fromGraph graph }

testGrid :: Effect Unit
testGrid = testFromGraph
