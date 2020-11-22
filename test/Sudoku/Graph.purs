module Test.Sudoku.Graph
  ( testGraph
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assert, assertEqual)
import Data.Array (length)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))

import Sudoku.VertexColor (VertexColor(..))
import Sudoku.Graph (from2dArray, rowCoords, colCoords, blockCoords, cliques, adjacentVertices, uncoloredVerticies)

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
  let expectedGraph = Map.fromFoldable [Tuple (Tuple 0 0) Six
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

testCliques :: Effect Unit
testCliques = do
  assertEqual { expected: 27, actual: Set.size cliques }
  let sampleRow = Set.fromFoldable [Tuple 3 0, Tuple 3 1, Tuple 3 2, Tuple 3 3, Tuple 3 4, Tuple 3 5, Tuple 3 6, Tuple 3 7, Tuple 3 8]
  let sampleCol = Set.fromFoldable [Tuple 0 5, Tuple 1 5, Tuple 2 5, Tuple 3 5, Tuple 4 5, Tuple 5 5, Tuple 6 5, Tuple 7 5, Tuple 8 5]
  let sampleBlock = Set.fromFoldable [Tuple 6 6, Tuple 6 7, Tuple 6 8
                                     ,Tuple 7 6, Tuple 7 7, Tuple 7 8
                                     ,Tuple 8 6, Tuple 8 7, Tuple 8 8]
  assert $ Set.member sampleRow cliques
  assert $ Set.member sampleCol cliques
  assert $ Set.member sampleBlock cliques

testAdjacentVertices :: Effect Unit
testAdjacentVertices = do
  let expectedVertices = Set.fromFoldable [(Tuple 0 1)
                                          ,(Tuple 0 2)
                                          ,(Tuple 0 3)
                                          ,(Tuple 0 4)
                                          ,(Tuple 0 5)
                                          ,(Tuple 0 6)
                                          ,(Tuple 0 7)
                                          ,(Tuple 0 8)
                                          ,(Tuple 1 0)
                                          ,(Tuple 1 1)
                                          ,(Tuple 1 2)
                                          ,(Tuple 2 0)
                                          ,(Tuple 2 1)
                                          ,(Tuple 2 2)
                                          ,(Tuple 3 0)
                                          ,(Tuple 4 0)
                                          ,(Tuple 5 0)
                                          ,(Tuple 6 0)
                                          ,(Tuple 7 0)
                                          ,(Tuple 8 0)]
  assertEqual { expected: expectedVertices, actual: adjacentVertices (Tuple 0 0) }

testUncoloredVertices :: Effect Unit
testUncoloredVertices = do
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
  let expectedUncolored = Set.fromFoldable [Tuple 0 3
                                           ,Tuple 0 5
                                           ,Tuple 1 0
                                           ,Tuple 1 1
                                           ,Tuple 1 2
                                           ,Tuple 1 3
                                           ,Tuple 1 6
                                           ,Tuple 1 7
                                           ,Tuple 2 0
                                           ,Tuple 2 1
                                           ,Tuple 2 3
                                           ,Tuple 2 6
                                           ,Tuple 2 8
                                           ,Tuple 3 8
                                           ,Tuple 4 2
                                           ,Tuple 4 5
                                           ,Tuple 4 6
                                           ,Tuple 4 8
                                           ,Tuple 5 0
                                           ,Tuple 5 1
                                           ,Tuple 5 3
                                           ,Tuple 5 4
                                           ,Tuple 5 8
                                           ,Tuple 6 0
                                           ,Tuple 6 1
                                           ,Tuple 6 2
                                           ,Tuple 6 3
                                           ,Tuple 6 4
                                           ,Tuple 6 6
                                           ,Tuple 6 8
                                           ,Tuple 7 2
                                           ,Tuple 7 3
                                           ,Tuple 7 5
                                           ,Tuple 7 8
                                           ,Tuple 8 5
                                           ,Tuple 8 7
                                           ,Tuple 8 8]
  assertEqual { expected: expectedUncolored, actual: uncoloredVerticies graph }

testGraph :: Effect Unit
testGraph = do
  testFrom2dArray
  testRowCoords
  testColCoords
  testBlockCoords
  testCliques
  testAdjacentVertices
  testUncoloredVertices
