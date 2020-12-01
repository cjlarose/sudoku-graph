module Test.Sudoku.SetUtils
  ( testSetUtils
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Set as Set

import Sudoku.SetUtils (kCombinations)

testKCombinations :: Effect Unit
testKCombinations = do
  let elements = Set.fromFoldable [2, 6, 9]
  let expectedPairs = Set.fromFoldable [ Set.fromFoldable [2, 6]
                                       , Set.fromFoldable [2, 9]
                                       , Set.fromFoldable [6, 9]
                                       ]
  assertEqual { expected: expectedPairs, actual: kCombinations 2 elements }

testSetUtils :: Effect Unit
testSetUtils = testKCombinations
