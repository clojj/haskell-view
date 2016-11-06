{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main

import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Sequence

import           Debug.Trace

import Lib
import PerformanceTextTest
import PerformanceByteStringTest
import PerformanceByteStringUTF8Test
import PerformanceSequenceOfCharTest
import PerformanceLoopOverCharsTest

import PerformanceLoopByteStringUTF8Test
import Data.ByteString.UTF8 as UTF8


main :: IO ()
main = do
  content <- readFile "test/testdata/TestMod.hs"
  let contentSeq = fromList content
  contentText <- TIO.readFile "test/testdata/TestMod.hs"
  contentByteString <- C.readFile "test/testdata/TestMod.hs"
  let contentByteStringUTF8 = UTF8.fromString content

  let tsText =
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
  -- TIO.writeFile "webclient/docroot/TestMod.out.text" resultText

  -- print "ByteString"
  -- let result = doByteString contentByteString ts
  -- C.putStr result
  -- C.writeFile "webclient/docroot/TestMod.out.bs" result

  print "ByteStringUTF8"
  let result = loopOverForElm contentByteStringUTF8 (Pos 1 1, mempty) tsString
  C.putStr result
  -- C.writeFile "webclient/docroot/TestMod.out.bs" result

  defaultMain [
    bgroup "insert tokens"
      [
        -- bench "Text" $ nf (doText contentText) tsText
      -- bench "ByteString" $ nf (doByteString contentByteString) ts
      bench "ByteStringUTF8" $ nf (loopOverForElm contentByteStringUTF8 (Pos 1 1, mempty)) tsString,
      -- bench "Seq Char" $ nf (doSeqChar contentSeq) tsSeq
      bench "loop over Char (in Text)" $ nf (doLoopOverChars contentText) tsString]
     ]
