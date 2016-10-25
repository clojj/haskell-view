{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Sequence as S
import           Data.Foldable

import           Debug.Trace

import Lib
import PerformanceTextTest
import PerformanceByteStringTest
import PerformanceByteStringUTF8Test
import PerformanceSequenceOfCharTest
import PerformanceFoldOverCharsTest


main :: IO ()
main = do
  content <- readFile "test/testdata/TestMod.hs"
  contentText <- TIO.readFile "test/testdata/TestMod.hs"
  contentByteString <- C.readFile "test/testdata/TestMod.hs"

  let ts =
        [((Pos 1 1,Pos 1 7), "ITmodule"),
        ((Pos 1 8,Pos 1 15), "ITconid"),
        ((Pos 2 5,Pos 2 6), "todo"),
        ((Pos 2 7,Pos 2 15), "todo"),
        ((Pos 3 5,Pos 3 6), "todo"),
        ((Pos 3 7,Pos 3 12), "todo"),
        ((Pos 5 1,Pos 5 1), "special"),
        ((Pos 5 1,Pos 5 9), "todo"),
        ((Pos 5 10,Pos 5 12), "todo"),
        ((Pos 5 13,Pos 5 15), "ITconid"),
        ((Pos 5 16,Pos 5 17), "todo"),
        ((Pos 5 17,Pos 5 18), "todo"),
        ((Pos 6 1,Pos 6 1), "todo"),
        ((Pos 6 1,Pos 6 9), "todo"),
        ((Pos 6 10,Pos 6 11), "todo"),
        ((Pos 6 12,Pos 6 20), "todo"),
        ((Pos 6 21,Pos 6 31), "todo"),
        ((Pos 8 1,Pos 10 5), "ITblockComment"),
        ((Pos 11 1,Pos 11 1), "todo"),
        ((Pos 11 1,Pos 11 6), "todo"),
        ((Pos 11 7,Pos 11 8), "todo"),
        ((Pos 11 9,Pos 13 8), "todo"),
        ((Pos 13 11,Pos 13 35), "todo"),
        ((Pos 14 1,Pos 14 1), "todo")]

  let tsString =
        [((Pos 1 1,Pos 1 7), "ITmodule"),
        ((Pos 1 8,Pos 1 15), "ITconid"),
        ((Pos 2 5,Pos 2 6), "todo"),
        ((Pos 2 7,Pos 2 15), "todo"),
        ((Pos 3 5,Pos 3 6), "todo"),
        ((Pos 3 7,Pos 3 12), "todo"),
        ((Pos 5 1,Pos 5 1), "special"),
        ((Pos 5 1,Pos 5 9), "todo"),
        ((Pos 5 10,Pos 5 12), "todo"),
        ((Pos 5 13,Pos 5 15), "ITconid"),
        ((Pos 5 16,Pos 5 17), "todo"),
        ((Pos 5 17,Pos 5 18), "todo"),
        ((Pos 6 1,Pos 6 1), "todo"),
        ((Pos 6 1,Pos 6 9), "todo"),
        ((Pos 6 10,Pos 6 11), "todo"),
        ((Pos 6 12,Pos 6 20), "todo"),
        ((Pos 6 21,Pos 6 31), "todo"),
        ((Pos 8 1,Pos 10 5), "ITblockComment"),
        ((Pos 11 1,Pos 11 1), "todo"),
        ((Pos 11 1,Pos 11 6), "todo"),
        ((Pos 11 7,Pos 11 8), "todo"),
        ((Pos 11 9,Pos 13 8), "todo"),
        ((Pos 13 11,Pos 13 35), "todo"),
        ((Pos 14 1,Pos 14 1), "todo")]

  -- print "Text"
  -- let resultText = doText contentText tsText
  -- TIO.putStr resultText

  -- print "ByteString"
  -- let result = doByteString contentByteString ts
  -- C.putStr result

  print "ByteStringUTF8"
  let result = doByteStringUTF8 contentByteString ts
  C.putStr result

  -- print "Seq Char"
  -- let result = doSeqChar (S.fromList content) ts
  -- putStrLn $ toList result

  print "fold over Char"
  let result = doFoldOverChars contentText tsString
  putStrLn $ toList result
