module Sudoku.VertexColor
  ( VertexColor(..)
  , allColors
  , fromInt
  , toInt
  ) where

import Prelude

import Data.Set as Set
import Data.Maybe (Maybe(..))

data VertexColor = One | Two | Three | Four | Five | Six | Seven | Eight | Nine

derive instance eqVertexColor :: Eq VertexColor
derive instance ordVertexColor :: Ord VertexColor

instance showVertexColor :: Show VertexColor where
  show One = "(VertexColor One)"
  show Two = "(VertexColor Two)"
  show Three = "(VertexColor Three)"
  show Four = "(VertexColor Four)"
  show Five = "(VertexColor Five)"
  show Six = "(VertexColor Six)"
  show Seven = "(VertexColor Seven)"
  show Eight = "(VertexColor Eight)"
  show Nine = "(VertexColor Nine)"

allColors :: Set.Set VertexColor
allColors = Set.fromFoldable [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

fromInt :: Int -> Maybe VertexColor
fromInt 1 = Just One
fromInt 2 = Just Two
fromInt 3 = Just Three
fromInt 4 = Just Four
fromInt 5 = Just Five
fromInt 6 = Just Six
fromInt 7 = Just Seven
fromInt 8 = Just Eight
fromInt 9 = Just Nine
fromInt _ = Nothing

toInt :: VertexColor -> Int
toInt One = 1
toInt Two = 2
toInt Three = 3
toInt Four = 4
toInt Five = 5
toInt Six = 6
toInt Seven = 7
toInt Eight = 8
toInt Nine = 9
