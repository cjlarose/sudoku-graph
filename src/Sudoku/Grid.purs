module Sudoku.Grid
  ( Grid
  , fromGraph
  , showGrid
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array ((..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)

import Sudoku.VertexColor (VertexColor)
import Sudoku.VertexColor as Color
import Sudoku.Graph (Graph)

type Grid = Array (Array (Maybe VertexColor))

fromGraph :: Graph -> Grid
fromGraph graph = map getRow $ 0 .. 8
  where
    getColor :: Int -> Int -> Maybe VertexColor
    getColor i j = Map.lookup (Tuple i j) graph 

    getRow :: Int -> Array (Maybe VertexColor)
    getRow i = map (getColor i) $ 0 .. 8

showCell :: Maybe VertexColor -> String
showCell Nothing = " "
showCell (Just color) = show <<< Color.toInt $ color

showGrid :: Grid -> String
showGrid [[aa, ab, ac, ad, ae, af, ag, ah, ai]
         ,[ba, bb, bc, bd, be, bf, bg, bh, bi]
         ,[ca, cb, cc, cd, ce, cf, cg, ch, ci]
         ,[da, db, dc, dd, de, df, dg, dh, di]
         ,[ea, eb, ec, ed, ee, ef, eg, eh, ei]
         ,[fa, fb, fc, fd, fe, ff, fg, fh, fi]
         ,[ga, gb, gc, gd, ge, gf, gg, gh, gi]
         ,[ha, hb, hc, hd, he, hf, hg, hh, hi]
         ,[ia, ib, ic, id, ie, ix, ig, ih, ii]] =
  "┌─────┬─────┬─────┐\n" <>
  "│" <> showCell aa <> " " <> showCell ab <> " " <> showCell ac <>
  "│" <> showCell ad <> " " <> showCell ae <> " " <> showCell af <>
  "│" <> showCell ag <> " " <> showCell ah <> " " <> showCell ai <>
  "│\n" <>
  "│" <> showCell ba <> " " <> showCell bb <> " " <> showCell bc <>
  "│" <> showCell bd <> " " <> showCell be <> " " <> showCell bf <>
  "│" <> showCell bg <> " " <> showCell bh <> " " <> showCell bi <>
  "│\n" <>
  "│" <> showCell ca <> " " <> showCell cb <> " " <> showCell cc <>
  "│" <> showCell cd <> " " <> showCell ce <> " " <> showCell cf <>
  "│" <> showCell cg <> " " <> showCell ch <> " " <> showCell ci <>
  "│\n" <>
  "├─────┼─────┼─────│\n" <>
  "│" <> showCell da <> " " <> showCell db <> " " <> showCell dc <>
  "│" <> showCell dd <> " " <> showCell de <> " " <> showCell df <>
  "│" <> showCell dg <> " " <> showCell dh <> " " <> showCell di <>
  "│\n" <>
  "│" <> showCell ea <> " " <> showCell eb <> " " <> showCell ec <>
  "│" <> showCell ed <> " " <> showCell ee <> " " <> showCell ef <>
  "│" <> showCell eg <> " " <> showCell eh <> " " <> showCell ei <>
  "│\n" <>
  "│" <> showCell fa <> " " <> showCell fb <> " " <> showCell fc <>
  "│" <> showCell fd <> " " <> showCell fe <> " " <> showCell ff <>
  "│" <> showCell fg <> " " <> showCell fh <> " " <> showCell fi <>
  "│\n" <>
  "├─────┼─────┼─────│\n" <>
  "│" <> showCell ga <> " " <> showCell gb <> " " <> showCell gc <>
  "│" <> showCell gd <> " " <> showCell ge <> " " <> showCell gf <>
  "│" <> showCell gg <> " " <> showCell gh <> " " <> showCell gi <>
  "│\n" <>
  "│" <> showCell ha <> " " <> showCell hb <> " " <> showCell hc <>
  "│" <> showCell hd <> " " <> showCell he <> " " <> showCell hf <>
  "│" <> showCell hg <> " " <> showCell hh <> " " <> showCell hi <>
  "│\n" <>
  "│" <> showCell ia <> " " <> showCell ib <> " " <> showCell ic <>
  "│" <> showCell id <> " " <> showCell ie <> " " <> showCell ix <>
  "│" <> showCell ig <> " " <> showCell ih <> " " <> showCell ii <>
  "│\n" <>
  "└─────┴─────┴─────┘"
showGrid _ = unsafeThrow "Malformed grid"

