module Test.Sudoku.MagicTour
  ( testMagicTour1465
  , main
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Node.FS.Sync as FSSync
import Node.Encoding (Encoding(..))
import Data.Maybe (Maybe(..))
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Control.Alt ((<|>))
import Text.Parsing.Parser (ParserT, runParser)
import Text.Parsing.Parser.Combinators (sepEndBy1)
import Text.Parsing.Parser.String (char, oneOf, eof)
import Data.Array (some, zipWith)
import Data.List (List)
import Data.Map as Map
import Data.Set as Set
import Data.Foldable (fold, for_)

import Sudoku.PartialColoring (PartialColoring(..), Coord, allCoords)
import Sudoku.VertexColor (VertexColor, fromInt)
import Sudoku.Worksheet (Worksheet(..), completeWithAnnotations, showWorksheet, showAnnotatedWorksheet)
import Sudoku.Solve (solveWorksheet)

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

problems :: forall m. Monad m => ParserT String m (List PartialColoring)
problems = sepEndBy1 problem (char '\n') <* eof

testMagicTour1465 :: Effect Unit
testMagicTour1465 = do
  let path = "./test/magic-tour-top-1465.txt"
  contents <- FSSync.readTextFile UTF8 path
  case runParser contents problems of
    Left err -> assert' ("error: " <> show err) false
    Right xs -> do
      for_ xs \coloring -> do
        let worksheet = Worksheet coloring
        log "Problem input:"
        log <<< showWorksheet $ worksheet
        let maybeSolved = solveWorksheet worksheet
        log "Final state:" 
        log <<< showAnnotatedWorksheet $ maybeSolved
        assert' "Not complete" <<< completeWithAnnotations $ maybeSolved

main :: Effect Unit
main = testMagicTour1465
