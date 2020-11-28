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
import Sudoku.Worksheet as Worksheet
import Sudoku.Worksheet (Worksheet(..), AnnotatedWorksheet(..), from2dArray, setVertexColor, setVertexColorWithAnnotations, showWorksheet, showAnnotatedWorksheet, addAnnotations)
import Sudoku.Solve (findCrossHatch, findNakedSingle)

printWorksheet :: Worksheet -> Effect Unit
printWorksheet = log <<< showWorksheet

printAnnotatedWorksheet :: AnnotatedWorksheet -> Effect Unit
printAnnotatedWorksheet = log <<< showAnnotatedWorksheet

suggestAndPromptWithAnnotations :: ReadLine.Interface -> AnnotatedWorksheet -> Effect Unit
suggestAndPromptWithAnnotations interface worksheet@(AnnotatedWorksheet ws) = do
  let result = findCrossHatch ws.coloring
  case result of
    Just suggestion@(Tuple coord@(Tuple i j) color) -> do
      log $ "Suggestion: Fill cell (" <> show i <> "," <> show j <> ")" <> " with value " <> show (Color.toInt color)
      let newWorksheet = setVertexColorWithAnnotations coord color worksheet
      printAnnotatedWorksheet newWorksheet
      if Worksheet.completeWithAnnotations newWorksheet
      then do
        log "Puzzle complete!"
        Process.exit 0
      else do
        log ""
        let handleLine line = if line == "y"
                              then suggestAndPromptWithAnnotations interface newWorksheet
                              else Process.exit 0
        ReadLine.question "Continue [yN]? " handleLine interface
    Nothing -> do
      let nakedSingle = findNakedSingle worksheet
      case nakedSingle of
        Just suggestion@(Tuple coord@(Tuple i j) color) -> do
          log $ "Suggestion (Naked Single): Fill cell (" <> show i <> "," <> show j <> ")" <> " with value " <> show (Color.toInt color)
          let newWorksheet = setVertexColorWithAnnotations coord color worksheet
          printAnnotatedWorksheet newWorksheet
          if Worksheet.completeWithAnnotations newWorksheet
          then do
            log "Puzzle complete!"
            Process.exit 0
          else do
            log ""
            let handleLine line = if line == "y"
                                  then suggestAndPromptWithAnnotations interface newWorksheet
                                  else Process.exit 0
            ReadLine.question "Continue [yN]? " handleLine interface
        Nothing -> do
          log "No suggestion"
          Process.exit 0

suggestAndPrompt :: ReadLine.Interface -> Worksheet -> Effect Unit
suggestAndPrompt interface worksheet@(Worksheet coloring) = do
  let result = findCrossHatch coloring
  case result of
    Just suggestion@(Tuple coord@(Tuple i j) color) -> do
      log $ "Suggestion: Fill cell (" <> show i <> "," <> show j <> ")" <> " with value " <> show (Color.toInt color)
      let newWorksheet = setVertexColor coord color worksheet
      printWorksheet newWorksheet
      if Worksheet.complete newWorksheet
      then do
        log "Puzzle complete!"
        Process.exit 0
      else do
        log ""
        let handleLine line = if line == "y"
                              then suggestAndPrompt interface newWorksheet
                              else Process.exit 0
        ReadLine.question "Continue [yN]? " handleLine interface
    Nothing -> do
      log "Add candidate annotations for every cell"
      let newWorksheet = addAnnotations worksheet
      printAnnotatedWorksheet newWorksheet
      log ""
      let handleLine line = if line == "y"
                            then suggestAndPromptWithAnnotations interface newWorksheet
                            else Process.exit 0
      ReadLine.question "Continue [yN]? " handleLine interface

main :: Effect Unit
main = do
  let worksheet = from2dArray [[9, 0, 0, 7, 5, 0, 0, 6, 8]
                              ,[0, 2, 0, 0, 0, 9, 5, 0, 1]
                              ,[6, 0, 7, 0, 3, 0, 2, 4, 0]
                              ,[0, 0, 0, 0, 6, 0, 4, 5, 0]
                              ,[0, 0, 5, 0, 2, 0, 0, 0, 3]
                              ,[0, 0, 0, 0, 0, 5, 6, 0, 0]
                              ,[0, 0, 0, 0, 0, 3, 1, 2, 0]
                              ,[0, 1, 4, 2, 8, 0, 0, 9, 0]
                              ,[0, 0, 0, 0, 7, 0, 0, 3, 0]]
  log "Input:"
  printWorksheet worksheet
  if Worksheet.complete worksheet
  then do
    log "Puzzle complete!"
    Process.exit 0
  else do
    log ""
    interface <- ReadLine.createInterface Process.stdin $ ReadLine.output := Process.stdout
    suggestAndPrompt interface worksheet
