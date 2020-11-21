module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Sudoku.Grid (fromIntGrid, showGrid)
import Sudoku.Graph (toGraph)

main :: Effect Unit
main = do
  let grid = fromIntGrid [[6, 8, 5, 0, 3, 0, 2, 9, 4]
                         ,[0, 0, 0, 0, 9, 2, 0, 0, 5]
                         ,[0, 0, 3, 0, 5, 6, 0, 7, 0]
                         ,[2, 1, 9, 6, 8, 3, 5, 4, 0]
                         ,[4, 5, 0, 9, 2, 0, 0, 8, 0]
                         ,[0, 0, 8, 0, 0, 5, 1, 2, 0]
                         ,[0, 0, 0, 0, 0, 9, 0, 1, 0]
                         ,[1, 9, 0, 0, 7, 0, 4, 6, 0]
                         ,[8, 7, 6, 3, 1, 0, 9, 0, 0]]
  log <<< showGrid $ grid
  let graph = toGraph grid
  logShow graph
