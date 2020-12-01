module Sudoku.SetUtils
  ( kCombinations
  , catMaybes
  ) where

import Prelude

import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldMap)

kCombinations :: forall a. Ord a => Int -> Set.Set a -> Set.Set (Set.Set a)
kCombinations 0 _ = Set.singleton Set.empty
kCombinations k elements =
  case Set.findMin elements of
    Nothing -> Set.empty
    Just minElement -> Set.union withMin withoutMin
      where
        withMin = Set.map (Set.insert minElement) <<< kCombinations (k - 1) <<< Set.delete minElement $ elements
        withoutMin = kCombinations k <<< Set.delete minElement $ elements

catMaybes :: forall a b. Foldable a => Ord b => a (Maybe b) -> Set.Set b
catMaybes xs = foldMap f xs
  where
    f Nothing = Set.empty
    f (Just x) = Set.singleton x
