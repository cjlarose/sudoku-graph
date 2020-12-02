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

import Sudoku.Worksheet (Worksheet, AnnotatedWorksheet, setVertexColor, setVertexColorWithAnnotations, removeCandidatesFromCoords, addAnnotations, stripAnnotations, completeWithAnnotations)
import Sudoku.Techniques (findCrossHatch, findNakedSingle, findHiddenSingle, findNakedNTuple, findClaimingVerticies)
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
  , toSuggestion "Naked Pair" $ findNakedNTuple 2
  , toSuggestion "Naked Triplet" $ findNakedNTuple 3
  , toSuggestion "Naked Quad" $ findNakedNTuple 4
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
    RemoveCandidates { coords: coords, colors: colors } -> removeCandidatesFromCoords coords colors

solveAnnotatedWorksheet :: AnnotatedWorksheet -> AnnotatedWorksheet
solveAnnotatedWorksheet ws | completeWithAnnotations ws = ws
solveAnnotatedWorksheet ws =
  case getSuggestionForAnnotatedWorksheet ws of
    Nothing -> ws
    Just suggestion -> solveAnnotatedWorksheet <<< applySuggestionToAnnotatedWorksheet suggestion $ ws

solveWorksheet :: Worksheet -> AnnotatedWorksheet
solveWorksheet = solveAnnotatedWorksheet <<< addAnnotations
