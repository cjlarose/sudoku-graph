module Test.Main where

import Prelude

import Effect (Effect)

import Test.Sudoku.PartialColoring (testPartialColoring)
import Test.Sudoku.Grid (testGrid)
import Test.Sudoku.Solve (testSolve)
import Test.Sudoku.SetUtils (testSetUtils)

main :: Effect Unit
main = do
  testPartialColoring
  testGrid
  testSolve
  testSetUtils
