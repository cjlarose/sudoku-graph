module Test.Sudoku.Grid
  ( testGrid
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Sudoku.Grid (fromGraph)

testFromGraph :: Effect Unit
testFromGraph = do
  let graph = Map.fromFoldable [Tuple (Tuple 0 0) 6
                               ,Tuple (Tuple 0 1) 8
                               ,Tuple (Tuple 0 2) 5
                               ,Tuple (Tuple 0 4) 3
                               ,Tuple (Tuple 0 6) 2
                               ,Tuple (Tuple 0 7) 9
                               ,Tuple (Tuple 0 8) 4
                               ,Tuple (Tuple 1 4) 9
                               ,Tuple (Tuple 1 5) 2
                               ,Tuple (Tuple 1 8) 5
                               ,Tuple (Tuple 2 2) 3
                               ,Tuple (Tuple 2 4) 5
                               ,Tuple (Tuple 2 5) 6
                               ,Tuple (Tuple 2 7) 7
                               ,Tuple (Tuple 3 0) 2
                               ,Tuple (Tuple 3 1) 1
                               ,Tuple (Tuple 3 2) 9
                               ,Tuple (Tuple 3 3) 6
                               ,Tuple (Tuple 3 4) 8
                               ,Tuple (Tuple 3 5) 3
                               ,Tuple (Tuple 3 6) 5
                               ,Tuple (Tuple 3 7) 4
                               ,Tuple (Tuple 4 0) 4
                               ,Tuple (Tuple 4 1) 5
                               ,Tuple (Tuple 4 3) 9
                               ,Tuple (Tuple 4 4) 2
                               ,Tuple (Tuple 4 7) 8
                               ,Tuple (Tuple 5 2) 8
                               ,Tuple (Tuple 5 5) 5
                               ,Tuple (Tuple 5 6) 1
                               ,Tuple (Tuple 5 7) 2
                               ,Tuple (Tuple 6 5) 9
                               ,Tuple (Tuple 6 7) 1
                               ,Tuple (Tuple 7 0) 1
                               ,Tuple (Tuple 7 1) 9
                               ,Tuple (Tuple 7 4) 7
                               ,Tuple (Tuple 7 6) 4
                               ,Tuple (Tuple 7 7) 6
                               ,Tuple (Tuple 8 0) 8
                               ,Tuple (Tuple 8 1) 7
                               ,Tuple (Tuple 8 2) 6
                               ,Tuple (Tuple 8 3) 3
                               ,Tuple (Tuple 8 4) 1
                               ,Tuple (Tuple 8 6) 9]
  let expectedGrid = [[(Just 6),(Just 8),(Just 5),Nothing,(Just 3),Nothing,(Just 2),(Just 9),(Just 4)]
                     ,[Nothing,Nothing,Nothing,Nothing,(Just 9),(Just 2),Nothing,Nothing,(Just 5)]
                     ,[Nothing,Nothing,(Just 3),Nothing,(Just 5),(Just 6),Nothing,(Just 7),Nothing]
                     ,[(Just 2),(Just 1),(Just 9),(Just 6),(Just 8),(Just 3),(Just 5),(Just 4),Nothing]
                     ,[(Just 4),(Just 5),Nothing,(Just 9),(Just 2),Nothing,Nothing,(Just 8),Nothing]
                     ,[Nothing,Nothing,(Just 8),Nothing,Nothing,(Just 5),(Just 1),(Just 2),Nothing]
                     ,[Nothing,Nothing,Nothing,Nothing,Nothing,(Just 9),Nothing,(Just 1),Nothing]
                     ,[(Just 1),(Just 9),Nothing,Nothing,(Just 7),Nothing,(Just 4),(Just 6),Nothing]
                     ,[(Just 8),(Just 7),(Just 6),(Just 3),(Just 1),Nothing,(Just 9),Nothing,Nothing]]
  assertEqual { expected: expectedGrid, actual: fromGraph graph }

testGrid :: Effect Unit
testGrid = testFromGraph
