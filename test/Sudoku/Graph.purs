module Test.Sudoku.Graph
  ( testGraph
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Array (length)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))

import Sudoku.Graph (from2dArray, rowCoords, colCoords, blockCoords)

testFrom2dArray :: Effect Unit
testFrom2dArray = do
  let matrix = [[6, 8, 5, 0, 3, 0, 2, 9, 4]
               ,[0, 0, 0, 0, 9, 2, 0, 0, 5]
               ,[0, 0, 3, 0, 5, 6, 0, 7, 0]
               ,[2, 1, 9, 6, 8, 3, 5, 4, 0]
               ,[4, 5, 0, 9, 2, 0, 0, 8, 0]
               ,[0, 0, 8, 0, 0, 5, 1, 2, 0]
               ,[0, 0, 0, 0, 0, 9, 0, 1, 0]
               ,[1, 9, 0, 0, 7, 0, 4, 6, 0]
               ,[8, 7, 6, 3, 1, 0, 9, 0, 0]]
  let expectedGraph = Map.fromFoldable [Tuple (Tuple 0 0) 6
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
  assertEqual { actual: from2dArray matrix, expected: expectedGraph }

testRowCoords :: Effect Unit
testRowCoords = do
  assertEqual { actual: length rowCoords, expected: 9 }
  let expectedRowCoords = [ Set.fromFoldable [Tuple 0 0, Tuple 0 1, Tuple 0 2, Tuple 0 3, Tuple 0 4, Tuple 0 5, Tuple 0 6, Tuple 0 7, Tuple 0 8]
                          , Set.fromFoldable [Tuple 1 0, Tuple 1 1, Tuple 1 2, Tuple 1 3, Tuple 1 4, Tuple 1 5, Tuple 1 6, Tuple 1 7, Tuple 1 8]
                          , Set.fromFoldable [Tuple 2 0, Tuple 2 1, Tuple 2 2, Tuple 2 3, Tuple 2 4, Tuple 2 5, Tuple 2 6, Tuple 2 7, Tuple 2 8]
                          , Set.fromFoldable [Tuple 3 0, Tuple 3 1, Tuple 3 2, Tuple 3 3, Tuple 3 4, Tuple 3 5, Tuple 3 6, Tuple 3 7, Tuple 3 8]
                          , Set.fromFoldable [Tuple 4 0, Tuple 4 1, Tuple 4 2, Tuple 4 3, Tuple 4 4, Tuple 4 5, Tuple 4 6, Tuple 4 7, Tuple 4 8]
                          , Set.fromFoldable [Tuple 5 0, Tuple 5 1, Tuple 5 2, Tuple 5 3, Tuple 5 4, Tuple 5 5, Tuple 5 6, Tuple 5 7, Tuple 5 8]
                          , Set.fromFoldable [Tuple 6 0, Tuple 6 1, Tuple 6 2, Tuple 6 3, Tuple 6 4, Tuple 6 5, Tuple 6 6, Tuple 6 7, Tuple 6 8]
                          , Set.fromFoldable [Tuple 7 0, Tuple 7 1, Tuple 7 2, Tuple 7 3, Tuple 7 4, Tuple 7 5, Tuple 7 6, Tuple 7 7, Tuple 7 8]
                          , Set.fromFoldable [Tuple 8 0, Tuple 8 1, Tuple 8 2, Tuple 8 3, Tuple 8 4, Tuple 8 5, Tuple 8 6, Tuple 8 7, Tuple 8 8] ]
  assertEqual { actual: rowCoords, expected: expectedRowCoords }

testColCoords :: Effect Unit
testColCoords = do
  assertEqual { actual: length colCoords, expected: 9 }
  let expectedColCoords = [ Set.fromFoldable [Tuple 0 0, Tuple 1 0, Tuple 2 0, Tuple 3 0, Tuple 4 0, Tuple 5 0, Tuple 6 0, Tuple 7 0, Tuple 8 0]
                          , Set.fromFoldable [Tuple 0 1, Tuple 1 1, Tuple 2 1, Tuple 3 1, Tuple 4 1, Tuple 5 1, Tuple 6 1, Tuple 7 1, Tuple 8 1]
                          , Set.fromFoldable [Tuple 0 2, Tuple 1 2, Tuple 2 2, Tuple 3 2, Tuple 4 2, Tuple 5 2, Tuple 6 2, Tuple 7 2, Tuple 8 2]
                          , Set.fromFoldable [Tuple 0 3, Tuple 1 3, Tuple 2 3, Tuple 3 3, Tuple 4 3, Tuple 5 3, Tuple 6 3, Tuple 7 3, Tuple 8 3]
                          , Set.fromFoldable [Tuple 0 4, Tuple 1 4, Tuple 2 4, Tuple 3 4, Tuple 4 4, Tuple 5 4, Tuple 6 4, Tuple 7 4, Tuple 8 4]
                          , Set.fromFoldable [Tuple 0 5, Tuple 1 5, Tuple 2 5, Tuple 3 5, Tuple 4 5, Tuple 5 5, Tuple 6 5, Tuple 7 5, Tuple 8 5]
                          , Set.fromFoldable [Tuple 0 6, Tuple 1 6, Tuple 2 6, Tuple 3 6, Tuple 4 6, Tuple 5 6, Tuple 6 6, Tuple 7 6, Tuple 8 6]
                          , Set.fromFoldable [Tuple 0 7, Tuple 1 7, Tuple 2 7, Tuple 3 7, Tuple 4 7, Tuple 5 7, Tuple 6 7, Tuple 7 7, Tuple 8 7]
                          , Set.fromFoldable [Tuple 0 8, Tuple 1 8, Tuple 2 8, Tuple 3 8, Tuple 4 8, Tuple 5 8, Tuple 6 8, Tuple 7 8, Tuple 8 8] ]
  assertEqual { actual: colCoords, expected: expectedColCoords }

testBlockCoords :: Effect Unit
testBlockCoords = do
  let expectedCoords = [ Set.fromFoldable [Tuple 0 0, Tuple 0 1, Tuple 0 2
                                          ,Tuple 1 0, Tuple 1 1, Tuple 1 2
                                          ,Tuple 2 0, Tuple 2 1, Tuple 2 2]
                       , Set.fromFoldable [Tuple 0 3, Tuple 0 4, Tuple 0 5
                                          ,Tuple 1 3, Tuple 1 4, Tuple 1 5
                                          ,Tuple 2 3, Tuple 2 4, Tuple 2 5]
                       , Set.fromFoldable [Tuple 0 6, Tuple 0 7, Tuple 0 8
                                          ,Tuple 1 6, Tuple 1 7, Tuple 1 8
                                          ,Tuple 2 6, Tuple 2 7, Tuple 2 8]
                       , Set.fromFoldable [Tuple 3 0, Tuple 3 1, Tuple 3 2
                                          ,Tuple 4 0, Tuple 4 1, Tuple 4 2
                                          ,Tuple 5 0, Tuple 5 1, Tuple 5 2]
                       , Set.fromFoldable [Tuple 3 3, Tuple 3 4, Tuple 3 5
                                          ,Tuple 4 3, Tuple 4 4, Tuple 4 5
                                          ,Tuple 5 3, Tuple 5 4, Tuple 5 5]
                       , Set.fromFoldable [Tuple 3 6, Tuple 3 7, Tuple 3 8
                                          ,Tuple 4 6, Tuple 4 7, Tuple 4 8
                                          ,Tuple 5 6, Tuple 5 7, Tuple 5 8]
                       , Set.fromFoldable [Tuple 6 0, Tuple 6 1, Tuple 6 2
                                          ,Tuple 7 0, Tuple 7 1, Tuple 7 2
                                          ,Tuple 8 0, Tuple 8 1, Tuple 8 2]
                       , Set.fromFoldable [Tuple 6 3, Tuple 6 4, Tuple 6 5
                                          ,Tuple 7 3, Tuple 7 4, Tuple 7 5
                                          ,Tuple 8 3, Tuple 8 4, Tuple 8 5]
                       , Set.fromFoldable [Tuple 6 6, Tuple 6 7, Tuple 6 8
                                          ,Tuple 7 6, Tuple 7 7, Tuple 7 8
                                          ,Tuple 8 6, Tuple 8 7, Tuple 8 8] ]
  assertEqual { expected: expectedCoords, actual: blockCoords }

testGraph :: Effect Unit
testGraph = do
  testFrom2dArray
  testRowCoords
  testColCoords
  testBlockCoords
