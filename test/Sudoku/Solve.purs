module Test.Sudoku.Solve
  ( testSolve
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Set as Set

import Sudoku.Solve (findCrossHatch, findNakedSingle, findHiddenSingle, findClaimingVerticies)
import Sudoku.VertexColor (VertexColor(..))
import Sudoku.Worksheet (addAnnotations)
import Sudoku.Worksheet as WS
import Sudoku.Suggestion (SuggestedAction(..))

testCrossHatchReturnsNothingWhenNoneAvailable :: Effect Unit
testCrossHatchReturnsNothingWhenNoneAvailable = do
  let graph = WS.from2dArray [[0, 0, 0, 0, 3, 0, 0, 0, 0]
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
  let graph = WS.from2dArray [[0, 0, 0, 0, 3, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 0, 3, 0]
                             ,[0, 0, 3, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 3, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 3, 0, 0]
                             ,[3, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 3, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 3, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just $ FillCell { coord: Tuple 7 8, color: Three }
  assertEqual { expected: expectedSuggestion, actual: findCrossHatch graph }

testCrossHatchReturnsSuggestionForColumn :: Effect Unit
testCrossHatchReturnsSuggestionForColumn = do
  let graph = WS.from2dArray [[0, 0, 0, 1, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 7, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 3, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 6, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 8, 0, 0]
                             ,[0, 0, 8, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 9, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 5, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just $ FillCell { coord: Tuple 4 3, color: Eight }
  assertEqual { expected: expectedSuggestion, actual: findCrossHatch graph }

testCrossHatchReturnsSuggestionForRow :: Effect Unit
testCrossHatchReturnsSuggestionForRow = do
  let graph = WS.from2dArray [[0, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 4, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 4, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[7, 0, 6, 0, 5, 2, 0, 9, 8]
                             ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                             ,[0, 0, 0, 0, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just $ FillCell { coord: Tuple 6 6, color: Four }
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
  let expectedSuggestion = Just $ FillCell { coord: Tuple 8 1, color: Nine }
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
  let expectedSuggestion = Just $ FillCell { coord: Tuple 6 6, color: Four }
  assertEqual { expected: expectedSuggestion, actual: findHiddenSingle worksheet }

testClaimingVerticiesInRow :: Effect Unit
testClaimingVerticiesInRow = do
  let worksheet = addAnnotations <<< WS.from2dArray $ [[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 8, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[5, 0, 4, 0, 0, 1, 2, 7, 6]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just $ RemoveCandidates { coords: [ Tuple 4 3
                                                             , Tuple 4 4
                                                             , Tuple 4 5
                                                             , Tuple 5 3
                                                             , Tuple 5 4
                                                             , Tuple 5 5
                                                             ]
                                                   , colors: Set.singleton Eight }
  assertEqual { expected: expectedSuggestion, actual: findClaimingVerticies worksheet }

testClaimingVerticiesInBlock :: Effect Unit
testClaimingVerticiesInBlock = do
  let worksheet = addAnnotations <<< WS.from2dArray $ [[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 1, 4, 6]
                                                      ,[0, 0, 0, 0, 0, 0, 5, 2, 8]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 3, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]
                                                      ,[0, 0, 0, 0, 0, 0, 0, 0, 0]]
  let expectedSuggestion = Just $ RemoveCandidates { coords: [ Tuple 5 0
                                                             , Tuple 5 1
                                                             , Tuple 5 2
                                                             , Tuple 5 3
                                                             , Tuple 5 4
                                                             , Tuple 5 5
                                                             ]
                                                   , colors: Set.singleton Seven }
  assertEqual { expected: expectedSuggestion, actual: findClaimingVerticies worksheet }

testSolve :: Effect Unit
testSolve = do
  testCrossHatchReturnsNothingWhenNoneAvailable
  testCrossHatchReturnsSuggestionForBlock
  testCrossHatchReturnsSuggestionForColumn
  testCrossHatchReturnsSuggestionForRow
  testNakedSingle
  testHiddenSingle
  testClaimingVerticiesInRow
  testClaimingVerticiesInBlock
