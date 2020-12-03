module Test.Sudoku.MagicTour
  ( testMagicTour1465
  , main
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))
import Data.Either (Either(..))
import Text.Parsing.Parser (runParser)
import Data.List (List, mapWithIndex)
import Data.Tuple (Tuple(..))
import Data.Foldable (for_)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Simple as SimpleOutput
import Test.Unit.Assert as Assert

import Sudoku.PartialColoring (PartialColoring)
import Sudoku.Worksheet (Worksheet(..), completeWithAnnotations)
import Sudoku.Solve (solveWorksheet)
import Sudoku.Parse (problems)

testMagicTour1465 :: List PartialColoring -> TestSuite
testMagicTour1465 colorings = suite "magic tour" do
  for_ (mapWithIndex Tuple colorings) \(Tuple i coloring) -> do
    test ("problem " <> show i) do
      let worksheet = Worksheet coloring
      let maybeSolved = solveWorksheet worksheet
      Assert.assert "Not complete" <<< completeWithAnnotations $ maybeSolved

main :: Effect Unit
main = launchAff_ do
  let path = "./test/magic-tour-top-1465.txt"
  contents <- FS.readTextFile UTF8 path
  case runParser contents problems of
    Left err -> Assert.assert ("error: " <> show err) false
    Right colorings -> runTestWith SimpleOutput.runTest <<< testMagicTour1465 $ colorings
