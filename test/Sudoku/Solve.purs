module Test.Sudoku.Solve
  ( testSolve
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Sudoku.PartialColoring (from2dArray)
import Sudoku.Solve (tryCrossHatch)
import Sudoku.VertexColor (VertexColor(..))

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
  assertEqual { expected: expectedSuggestion, actual: tryCrossHatch graph }

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
  assertEqual { expected: expectedSuggestion, actual: tryCrossHatch graph }

testCrossHatchReturnsSuggestionForColumn :: Effect Unit
testCrossHatchReturnsSuggestionForColumn = do
  let graph = from2dArray [[0, 0, 0, 1, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 7, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 3, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 6, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 4, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 2, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 9, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 5, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just (Tuple (Tuple 4 3) Eight)
  assertEqual { expected: expectedSuggestion, actual: tryCrossHatch graph }

testCrossHatchReturnsSuggestionForRow :: Effect Unit
testCrossHatchReturnsSuggestionForRow = do
  let graph = from2dArray [[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[7, 1, 6, 3, 5, 2, 0, 9, 8]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                          ,[0, 0, 0, 0, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just (Tuple (Tuple 6 6) Four)
  assertEqual { expected: expectedSuggestion, actual: tryCrossHatch graph }

testSolve :: Effect Unit
testSolve = do
  testCrossHatchReturnsNothingWhenNoneAvailable
  testCrossHatchReturnsSuggestionForBlock
  testCrossHatchReturnsSuggestionForColumn
  testCrossHatchReturnsSuggestionForRow
