module Test.Sudoku.Solve
  ( testSolve
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Sudoku.PartialColoring (from2dArray)
import Sudoku.Solve (findCrossHatch, findNakedSingle, findHiddenSingle)
import Sudoku.VertexColor (VertexColor(..))
import Sudoku.Worksheet (addAnnotations)
import Sudoku.Worksheet as WS

testCrossHatchReturnsNothingWhenNoneAvailable :: Effect Unit
testCrossHatchReturnsNothingWhenNoneAvailable = do
  let graph = from2dArray [[0, 0, 0, 0, 3, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 3, 0]
                          ,[0, 0, 3, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 3, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 3, 0, 0]
                          ,[3, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 3, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 3]
                          ,[0, 0, 0, 3, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Nothing
  assertEqual { expected: expectedSuggestion, actual: findCrossHatch graph }

testCrossHatchReturnsSuggestionForBlock :: Effect Unit
testCrossHatchReturnsSuggestionForBlock = do
  let graph = from2dArray [[0, 0, 0, 0, 3, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 3, 0]
                          ,[0, 0, 3, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 3, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 3, 0, 0]
                          ,[3, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 3, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 3, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just (Tuple (Tuple 7 8) Three)
  assertEqual { expected: expectedSuggestion, actual: findCrossHatch graph }

testCrossHatchReturnsSuggestionForColumn :: Effect Unit
testCrossHatchReturnsSuggestionForColumn = do
  let graph = from2dArray [[0, 0, 0, 1, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 7, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 3, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 6, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 8, 0, 0]
                          ,[0, 0, 8, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 9, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 5, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just (Tuple (Tuple 4 3) Eight)
  assertEqual { expected: expectedSuggestion, actual: findCrossHatch graph }

testCrossHatchReturnsSuggestionForRow :: Effect Unit
testCrossHatchReturnsSuggestionForRow = do
  let graph = from2dArray [[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 4, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 4, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[7, 0, 6, 0, 5, 2, 0, 9, 8]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just (Tuple (Tuple 6 6) Four)
  assertEqual { expected: expectedSuggestion, actual: findCrossHatch graph }

testNakedSingle :: Effect Unit
testNakedSingle = do
  let worksheet = addAnnotations <<< WS.from2dArray $ [[0, 4, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 2, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 5, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 6, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 1, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 7, 0, 8, 3, 0]]
  let expectedSuggestion = Just (Tuple (Tuple 8 1) Nine)
  assertEqual { expected: expectedSuggestion, actual: findNakedSingle worksheet }

testHiddenSingle :: Effect Unit
testHiddenSingle = do
  let worksheet = addAnnotations <<< WS.from2dArray $ [[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 4, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 4, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[7, 0, 6, 0, 5, 2, 0, 9, 8]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just (Tuple (Tuple 6 6) Four)
  assertEqual { expected: expectedSuggestion, actual: findHiddenSingle worksheet }

testSolve :: Effect Unit
testSolve = do
  testCrossHatchReturnsNothingWhenNoneAvailable
  testCrossHatchReturnsSuggestionForBlock
  testCrossHatchReturnsSuggestionForColumn
  testCrossHatchReturnsSuggestionForRow
  testNakedSingle
  testHiddenSingle
