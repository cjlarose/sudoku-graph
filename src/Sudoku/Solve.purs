module Sudoku.Solve
  ( getSuggestion
  , getSuggestionForAnnotatedWorksheet
  , applySuggestion
  , applySuggestionToAnnotatedWorksheet
  , solveWorksheet
  ) where

import Prelude

import Effect.Exception.Unsafe (unsafeThrow)
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.List as List

import Sudoku.Worksheet (Worksheet, AnnotatedWorksheet, setVertexColor, setVertexColorWithAnnotations, removeCandidatesFromCoords, addAnnotations, completeWithAnnotations)
import Sudoku.Techniques (findCrossHatch, findNakedSingle, findHiddenSingle, findHiddenNTuple, findNakedNTuple, findClaimingVerticies)
import Sudoku.Suggestion (Suggestion, SuggestedAction(..))

getSuggestion :: Worksheet -> Maybe Suggestion
getSuggestion worksheet = do
  action <- findCrossHatch worksheet
  pure $ { strategyName: "Cross-Hatching", action: action }

applySuggestion :: Suggestion -> Worksheet -> Worksheet
applySuggestion { action: action } =
  case action of
    FillCell { coord: coord, color: color } -> setVertexColor coord color
    _ -> unsafeThrow "Impossible"

annotatedWorksheetStrategies :: List (AnnotatedWorksheet -> Maybe Suggestion)
annotatedWorksheetStrategies = List.fromFoldable $
  [ toSuggestion "Naked Single" findNakedSingle
  , toSuggestion "Hidden Single" findHiddenSingle
  , toSuggestion "Claiming" findClaimingVerticies
  , toSuggestion "Naked Pair" $ findNakedNTuple 2
  , toSuggestion "Naked Triplet" $ findNakedNTuple 3
  , toSuggestion "Naked Quad" $ findNakedNTuple 4
  , toSuggestion "Hidden Pair" $ findHiddenNTuple 2
  , toSuggestion "Hidden Triplet" $ findHiddenNTuple 3
  , toSuggestion "Hidden Quad" $ findHiddenNTuple 4
  ]
  where
    toSuggestion :: String -> (AnnotatedWorksheet -> Maybe SuggestedAction) -> AnnotatedWorksheet -> Maybe Suggestion
    toSuggestion name f x =
      (\action -> { strategyName: name, action: action }) <$> f x

getSuggestionForAnnotatedWorksheet :: AnnotatedWorksheet -> Maybe Suggestion
getSuggestionForAnnotatedWorksheet ws = go annotatedWorksheetStrategies
  where
    go (Cons f fs) = case f ws of
                       Just suggestion -> Just suggestion
                       Nothing -> go fs
    go Nil = Nothing


applySuggestionToAnnotatedWorksheet :: Suggestion -> AnnotatedWorksheet -> AnnotatedWorksheet
applySuggestionToAnnotatedWorksheet { action: action } =
  case action of
    FillCell { coord: coord, color: color} -> setVertexColorWithAnnotations coord color
    RemoveCandidates { coords: coords, colors: colors } -> removeCandidatesFromCoords coords colors

solveAnnotatedWorksheet :: AnnotatedWorksheet -> AnnotatedWorksheet
solveAnnotatedWorksheet ws | completeWithAnnotations ws = ws
solveAnnotatedWorksheet ws =
  case getSuggestionForAnnotatedWorksheet ws of
    Nothing -> ws
    Just suggestion -> solveAnnotatedWorksheet <<< applySuggestionToAnnotatedWorksheet suggestion $ ws

solveWorksheet :: Worksheet -> AnnotatedWorksheet
solveWorksheet = solveAnnotatedWorksheet <<< addAnnotations
