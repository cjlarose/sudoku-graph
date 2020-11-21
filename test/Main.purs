module Test.Main where

import Prelude

import Effect (Effect)

import Test.Sudoku.Graph (testGraph)
import Test.Sudoku.Grid (testGrid)

main :: Effect Unit
main = do
  testGraph
  testGrid
