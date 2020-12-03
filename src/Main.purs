module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, errorShow)
import Data.Options ((:=))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Node.Process as Process
import Node.ReadLine as ReadLine
import Text.Parsing.Parser (runParser)

import Sudoku.Worksheet as Worksheet
import Sudoku.Worksheet (Worksheet, AnnotatedWorksheet, showWorksheet, showAnnotatedWorksheet, addAnnotations, fromColoring)
import Sudoku.Suggestion (showSuggestion)
import Sudoku.Solve (getSuggestion, getSuggestionForAnnotatedWorksheet, applySuggestion, applySuggestionToAnnotatedWorksheet)
import Sudoku.Parse (singleProblem)

printWorksheet :: Worksheet -> Effect Unit
printWorksheet = log <<< showWorksheet

printAnnotatedWorksheet :: AnnotatedWorksheet -> Effect Unit
printAnnotatedWorksheet = log <<< showAnnotatedWorksheet

suggestAndPromptWithAnnotations :: ReadLine.Interface -> AnnotatedWorksheet -> Effect Unit
suggestAndPromptWithAnnotations interface worksheet = do
  let result = getSuggestionForAnnotatedWorksheet worksheet
  case result of
    Just suggestion -> do
      log <<< showSuggestion $ suggestion
      let newWorksheet = applySuggestionToAnnotatedWorksheet suggestion worksheet
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
suggestAndPrompt interface worksheet = do
  let result = getSuggestion worksheet
  case result of
    Just suggestion -> do
      log <<< showSuggestion $ suggestion
      let newWorksheet = applySuggestion suggestion worksheet
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

handlePuzzleInput :: ReadLine.Interface -> String -> Effect Unit
handlePuzzleInput interface input = do
  case runParser input singleProblem of
    Left err -> do
      errorShow err
      Process.exit 1
    Right coloring -> do
      let worksheet = fromColoring coloring
      suggestAndPrompt interface worksheet

main :: Effect Unit
main = do
  interface <- ReadLine.createInterface Process.stdin $ ReadLine.output := Process.stdout
  ReadLine.question "Enter puzzle\n" (handlePuzzleInput interface) interface
