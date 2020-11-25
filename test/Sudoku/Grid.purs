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
import Sudoku.Grid (fromPartialColoring)

testFromGraph :: Effect Unit
testFromGraph = do
  let graph = Map.fromFoldable [Tuple (Tuple 0 0) Six
                               ,Tuple (Tuple 0 1) Eight
                               ,Tuple (Tuple 0 2) Five
                               ,Tuple (Tuple 0 4) Three
                               ,Tuple (Tuple 0 6) Two
                               ,Tuple (Tuple 0 7) Nine
                               ,Tuple (Tuple 0 8) Four
                               ,Tuple (Tuple 1 4) Nine
                               ,Tuple (Tuple 1 5) Two
                               ,Tuple (Tuple 1 8) Five
                               ,Tuple (Tuple 2 2) Three
                               ,Tuple (Tuple 2 4) Five
                               ,Tuple (Tuple 2 5) Six
                               ,Tuple (Tuple 2 7) Seven
                               ,Tuple (Tuple 3 0) Two
                               ,Tuple (Tuple 3 1) One
                               ,Tuple (Tuple 3 2) Nine
                               ,Tuple (Tuple 3 3) Six
                               ,Tuple (Tuple 3 4) Eight
                               ,Tuple (Tuple 3 5) Three
                               ,Tuple (Tuple 3 6) Five
                               ,Tuple (Tuple 3 7) Four
                               ,Tuple (Tuple 4 0) Four
                               ,Tuple (Tuple 4 1) Five
                               ,Tuple (Tuple 4 3) Nine
                               ,Tuple (Tuple 4 4) Two
                               ,Tuple (Tuple 4 7) Eight
                               ,Tuple (Tuple 5 2) Eight
                               ,Tuple (Tuple 5 5) Five
                               ,Tuple (Tuple 5 6) One
                               ,Tuple (Tuple 5 7) Two
                               ,Tuple (Tuple 6 5) Nine
                               ,Tuple (Tuple 6 7) One
                               ,Tuple (Tuple 7 0) One
                               ,Tuple (Tuple 7 1) Nine
                               ,Tuple (Tuple 7 4) Seven
                               ,Tuple (Tuple 7 6) Four
                               ,Tuple (Tuple 7 7) Six
                               ,Tuple (Tuple 8 0) Eight
                               ,Tuple (Tuple 8 1) Seven
                               ,Tuple (Tuple 8 2) Six
                               ,Tuple (Tuple 8 3) Three
                               ,Tuple (Tuple 8 4) One
                               ,Tuple (Tuple 8 6) Nine]
  let expectedGrid = [[(Just Six),(Just Eight),(Just Five),Nothing,(Just Three),Nothing,(Just Two),(Just Nine),(Just Four)]
                     ,[Nothing,Nothing,Nothing,Nothing,(Just Nine),(Just Two),Nothing,Nothing,(Just Five)]
                     ,[Nothing,Nothing,(Just Three),Nothing,(Just Five),(Just Six),Nothing,(Just Seven),Nothing]
                     ,[(Just Two),(Just One),(Just Nine),(Just Six),(Just Eight),(Just Three),(Just Five),(Just Four),Nothing]
                     ,[(Just Four),(Just Five),Nothing,(Just Nine),(Just Two),Nothing,Nothing,(Just Eight),Nothing]
                     ,[Nothing,Nothing,(Just Eight),Nothing,Nothing,(Just Five),(Just One),(Just Two),Nothing]
                     ,[Nothing,Nothing,Nothing,Nothing,Nothing,(Just Nine),Nothing,(Just One),Nothing]
                     ,[(Just One),(Just Nine),Nothing,Nothing,(Just Seven),Nothing,(Just Four),(Just Six),Nothing]
                     ,[(Just Eight),(Just Seven),(Just Six),(Just Three),(Just One),Nothing,(Just Nine),Nothing,Nothing]]
  assertEqual { expected: expectedGrid, actual: fromPartialColoring graph }

testGrid :: Effect Unit
testGrid = testFromGraph
