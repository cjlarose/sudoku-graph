module Test.Main where

import Prelude

import Effect (Effect)

import Test.Sudoku.Graph (testGraph)

main :: Effect Unit
main = do
  testGraph
