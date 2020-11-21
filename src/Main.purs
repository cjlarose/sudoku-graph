module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Sudoku.Grid (fromGraph, showGrid)
import Sudoku.Graph (from2dArray)
import Sudoku.Solve (tryCrossHatch)

main :: Effect Unit
main = do
  let graph = from2dArray [[6, 8, 5, 0, 3, 0, 2, 9, 4]
                          ,[0, 0, 0, 0, 9, 2, 0, 0, 5]
                          ,[0, 0, 3, 0, 5, 6, 0, 7, 0]
                          ,[2, 1, 9, 6, 8, 3, 5, 4, 0]
                          ,[4, 5, 0, 9, 2, 0, 0, 8, 0]
                          ,[0, 0, 8, 0, 0, 5, 1, 2, 0]
                          ,[0, 0, 0, 0, 0, 9, 0, 1, 0]
                          ,[1, 9, 0, 0, 7, 0, 4, 6, 0]
                          ,[8, 7, 6, 3, 1, 0, 9, 0, 0]]
  let grid = fromGraph graph
  log <<< showGrid $ grid
  let result = tryCrossHatch graph
  logShow result
