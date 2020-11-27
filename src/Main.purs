module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Options ((:=))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Node.Process as Process
import Node.ReadLine as ReadLine

import Sudoku.VertexColor as Color
import Sudoku.Worksheet (Worksheet(..), AnnotatedWorksheet(..), from2dArray, setVertexColor, showWorksheet, showAnnotatedWorksheet, addAnnotations)
import Sudoku.Solve (tryCrossHatch)

printWorksheet :: Worksheet -> Effect Unit
printWorksheet = log <<< showWorksheet

printAnnotatedWorksheet :: AnnotatedWorksheet -> Effect Unit
printAnnotatedWorksheet = log <<< showAnnotatedWorksheet

suggestAndPromptWithAnnotations :: ReadLine.Interface -> AnnotatedWorksheet -> Effect Unit
suggestAndPromptWithAnnotations interface worksheet@(AnnotatedWorksheet coloring) = do
  log "Just do it"
  Process.exit 0

suggestAndPrompt :: ReadLine.Interface -> Worksheet -> Effect Unit
suggestAndPrompt interface worksheet@(Worksheet coloring) = do
  let result = tryCrossHatch coloring
  case result of
    Nothing -> do
      log "Add candidate annotations for every cell"
      let newWorksheet = addAnnotations worksheet
      printAnnotatedWorksheet newWorksheet
      log ""
      let handleLine line = if line == "y"
                            then suggestAndPromptWithAnnotations interface newWorksheet
                            else Process.exit 0
      ReadLine.question "Continue [yN]? " handleLine interface
    Just suggestion@(Tuple coord@(Tuple i j) color) -> do
      log $ "Suggestion: Fill cell (" <> show i <> "," <> show j <> ")" <> " with value " <> show (Color.toInt color)
      let newWorksheet = setVertexColor coord color worksheet
      printWorksheet newWorksheet
      log ""
      let handleLine line = if line == "y"
                            then suggestAndPrompt interface newWorksheet
                            else Process.exit 0
      ReadLine.question "Continue [yN]? " handleLine interface

main :: Effect Unit
main = do
  let worksheet = from2dArray [[6, 8, 5, 0, 3, 0, 2, 9, 4]
                              ,[0, 0, 0, 0, 9, 2, 0, 0, 5]
                              ,[0, 0, 3, 0, 5, 6, 0, 7, 0]
                              ,[2, 1, 9, 6, 8, 3, 5, 4, 0]
                              ,[4, 5, 0, 9, 2, 0, 0, 8, 0]
                              ,[0, 0, 8, 0, 0, 5, 1, 2, 0]
                              ,[0, 0, 0, 0, 0, 9, 0, 1, 0]
                              ,[1, 9, 0, 0, 7, 0, 4, 6, 0]
                              ,[8, 7, 6, 3, 1, 0, 9, 0, 0]]
  log "Input:"
  printWorksheet worksheet
  log ""
  interface <- ReadLine.createInterface Process.stdin $ ReadLine.output := Process.stdout
  suggestAndPrompt interface worksheet
