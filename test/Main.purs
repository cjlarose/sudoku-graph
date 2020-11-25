module Test.Main where

import Prelude

import Effect (Effect)

import Test.Sudoku.Graph (testGraph)
import Test.Sudoku.Grid (testGrid)
import Test.Sudoku.Solve (testSolve)

main :: Effect Unit
main = do
  testGraph
  testGrid
  testSolve
