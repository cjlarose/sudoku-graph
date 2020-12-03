module Sudoku.Parse
  ( singleProblem
  , problems
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Char (toCharCode)
import Control.Alt ((<|>))
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (sepEndBy1)
import Text.Parsing.Parser.String (char, oneOf, eof)
import Data.Array (some, zipWith)
import Data.List (List)
import Data.Map as Map
import Data.Set as Set
import Data.Foldable (fold)

import Sudoku.PartialColoring (PartialColoring(..), Coord, allCoords)
import Sudoku.VertexColor (VertexColor, fromInt)

emptyCell :: forall m. Monad m => ParserT String m (Maybe VertexColor)
emptyCell = Nothing <$ char '.'

cellWithClue :: forall m. Monad m => ParserT String m (Maybe VertexColor)
cellWithClue = fromInt <<< (\c -> toCharCode c - toCharCode '0') <$> oneOf ['1', '2', '3', '4', '5', '6', '7', '8', '9']

cellContent :: forall m. Monad m => ParserT String m (Maybe VertexColor)
cellContent = emptyCell <|> cellWithClue

problem :: forall m. Monad m => ParserT String m PartialColoring
problem = toPartialColoring <$> some cellContent
  where
    toColoring :: Coord -> Maybe VertexColor -> Map.Map Coord VertexColor
    toColoring _ Nothing = Map.empty
    toColoring coord (Just color) = Map.insert coord color Map.empty

    toPartialColoring :: Array (Maybe VertexColor) -> PartialColoring
    toPartialColoring xs = PartialColoring <<< fold $ zipWith (\coord content -> toColoring coord content) (Set.toUnfoldable allCoords) xs

singleProblem :: forall m. Monad m => ParserT String m PartialColoring
singleProblem = problem <* eof

problems :: forall m. Monad m => ParserT String m (List PartialColoring)
problems = sepEndBy1 problem (char '\n') <* eof
