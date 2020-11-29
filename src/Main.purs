module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Options ((:=))
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.List as List

import Node.Process as Process
import Node.ReadLine as ReadLine

import Sudoku.Worksheet as Worksheet
import Sudoku.Worksheet (Worksheet, AnnotatedWorksheet, from2dArray, setVertexColor, setVertexColorWithAnnotations, removeCandidateFromCoords, showWorksheet, showAnnotatedWorksheet, addAnnotations, stripAnnotations)
import Sudoku.Solve (findCrossHatch, findNakedSingle, findHiddenSingle, findClaimingVerticies)
import Sudoku.Suggestion (Suggestion, SuggestedAction(..), showSuggestion)

printWorksheet :: Worksheet -> Effect Unit
printWorksheet = log <<< showWorksheet

printAnnotatedWorksheet :: AnnotatedWorksheet -> Effect Unit
printAnnotatedWorksheet = log <<< showAnnotatedWorksheet

getSuggestion :: Worksheet -> Maybe Suggestion
getSuggestion worksheet = do
  action <- findCrossHatch worksheet
  pure $ { strategyName: "Cross-Hatching", action: action }

applySuggestion :: Suggestion -> Worksheet -> Worksheet
applySuggestion { action: action } =
  case action of
    FillCell { coord: coord, color: color } -> setVertexColor coord color
    _ -> unsafeThrow "Impossible"

findJust :: forall a b. List (a -> Maybe b) -> a -> Maybe b
findJust Nil _ = Nothing
findJust (Cons f fs) x = case f x of
                           Just y -> Just y
                           Nothing -> findJust fs x

annotatedWorksheetStrategies :: List (AnnotatedWorksheet -> Maybe Suggestion)
annotatedWorksheetStrategies = List.fromFoldable $
  [ toSuggestion "Cross-Hatching" $ findCrossHatch <<< stripAnnotations
  , toSuggestion "Naked Single" findNakedSingle
  , toSuggestion "Hidden Single" findHiddenSingle
  , toSuggestion "Claiming" findClaimingVerticies
  ]
  where
    toSuggestion :: String -> (AnnotatedWorksheet -> Maybe SuggestedAction) -> AnnotatedWorksheet -> Maybe Suggestion
    toSuggestion name f x =
      (\action -> { strategyName: name, action: action }) <$> f x

getSuggestionForAnnotatedWorksheet :: AnnotatedWorksheet -> Maybe Suggestion
getSuggestionForAnnotatedWorksheet = findJust annotatedWorksheetStrategies

applySuggestionToAnnotatedWorksheet :: Suggestion -> AnnotatedWorksheet -> AnnotatedWorksheet
applySuggestionToAnnotatedWorksheet { action: action } =
  case action of
    FillCell { coord: coord, color: color} -> setVertexColorWithAnnotations coord color
    RemoveCandidates { coords: coords, color: color } -> removeCandidateFromCoords coords color

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
