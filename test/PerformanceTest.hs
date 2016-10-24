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
import PerformanceFoldOverCharsTest


main :: IO ()
main = do
  content <- readFile "test/testdata/TestMod.hs"
  let contentSeq = fromList content
  contentText <- TIO.readFile "test/testdata/TestMod.hs"
  contentByteString <- C.readFile "test/testdata/TestMod.hs"

  let ts = [(((1,1),(1,7)), "ITmodule"),
            (((1,8),(1,15)), "ITconid"),
            (((2,5),(2,6)), "todo"),
            (((2,7),(2,15)), "todo"),
            (((3,5),(3,6)), "todo"),
            (((3,7),(3,12)), "todo"),
            (((5,1),(5,1)), "special"),
            (((5,1),(5,9)), "todo"),
            (((5,10),(5,12)), "todo"),
            (((5,13),(5,15)), "ITconid"),
            (((5,16),(5,17)), "todo"),
            (((5,17),(5,18)), "todo"),
            (((6,1),(6,1)), "todo"),
            (((6,1),(6,9)), "todo"),
            (((6,10),(6,11)), "todo"),
            (((6,12),(6,20)), "todo"),
            (((6,21),(6,31)), "todo"),
            (((8,1),(10,5)), "ITblockComment"),
            (((11,1),(11,1)), "todo"),
            (((11,1),(11,6)), "todo"),
            (((11,7),(11,8)), "todo"),
            (((11,9),(13,8)), "todo"),
            (((13,11),(13,35)), "todo"),
            (((14,1),(14,1)), "todo") ]

  let tsText = [(((1,1),(1,7)), "ITmodule"),
                (((1,8),(1,15)), "ITconid"),
                (((2,5),(2,6)), "todo"),
                (((2,7),(2,15)), "todo"),
                (((3,5),(3,6)), "todo"),
                (((3,7),(3,12)), "todo"),
                (((5,1),(5,1)), "special"),
                (((5,1),(5,9)), "todo"),
                (((5,10),(5,12)), "todo"),
                (((5,13),(5,15)), "ITconid"),
                (((5,16),(5,17)), "todo"),
                (((5,17),(5,18)), "todo"),
                (((6,1),(6,1)), "todo"),
                (((6,1),(6,9)), "todo"),
                (((6,10),(6,11)), "todo"),
                (((6,12),(6,20)), "todo"),
                (((6,21),(6,31)), "todo"),
                (((8,1),(10,5)), "ITblockComment"),
                (((11,1),(11,1)), "todo"),
                (((11,1),(11,6)), "todo"),
                (((11,7),(11,8)), "todo"),
                (((11,9),(13,8)), "todo"),
                (((13,11),(13,35)), "todo"),
                (((14,1),(14,1)), "todo")]

  let tsSeq = [(((1,1),(1,7)), "ITmodule"),
                (((1,8),(1,15)), "ITconid"),
                (((2,5),(2,6)), "todo"),
                (((2,7),(2,15)), "todo"),
                (((3,5),(3,6)), "todo"),
                (((3,7),(3,12)), "todo"),
                (((5,1),(5,1)), "special"),
                (((5,1),(5,9)), "todo"),
                (((5,10),(5,12)), "todo"),
                (((5,13),(5,15)), "ITconid"),
                (((5,16),(5,17)), "todo"),
                (((5,17),(5,18)), "todo"),
                (((6,1),(6,1)), "todo"),
                (((6,1),(6,9)), "todo"),
                (((6,10),(6,11)), "todo"),
                (((6,12),(6,20)), "todo"),
                (((6,21),(6,31)), "todo"),
                (((8,1),(10,5)), "ITblockComment"),
                (((11,1),(11,1)), "todo"),
                (((11,1),(11,6)), "todo"),
                (((11,7),(11,8)), "todo"),
                (((11,9),(13,8)), "todo"),
                (((13,11),(13,35)), "todo"),
                (((14,1),(14,1)), "todo")]

  let tsString = [(((1,1),(1,7)), "ITmodule"),
                (((1,8),(1,15)), "ITconid"),
                (((2,5),(2,6)), "todo"),
                (((2,7),(2,15)), "todo"),
                (((3,5),(3,6)), "todo"),
                (((3,7),(3,12)), "todo"),
                (((5,1),(5,1)), "special"),
                (((5,1),(5,9)), "todo"),
                (((5,10),(5,12)), "todo"),
                (((5,13),(5,15)), "ITconid"),
                (((5,16),(5,17)), "todo"),
                (((5,17),(5,18)), "todo"),
                (((6,1),(6,1)), "todo"),
                (((6,1),(6,9)), "todo"),
                (((6,10),(6,11)), "todo"),
                (((6,12),(6,20)), "todo"),
                (((6,21),(6,31)), "todo"),
                (((8,1),(10,5)), "ITblockComment"),
                (((11,1),(11,1)), "todo"),
                (((11,1),(11,6)), "todo"),
                (((11,7),(11,8)), "todo"),
                (((11,9),(13,8)), "todo"),
                (((13,11),(13,35)), "todo"),
                (((14,1),(14,1)), "todo")]

  -- print "Text"
  -- let resultText = doText contentText tsText
  -- TIO.putStr resultText
  -- TIO.writeFile "webclient/docroot/TestMod.out.text" resultText

  -- print "ByteString"
  -- let result = doByteString contentByteString ts
  -- C.putStr result
  -- C.writeFile "webclient/docroot/TestMod.out.bs" result

  defaultMain [
    bgroup "insert tokens"
      [
    --  [ bench "Text" $ nf (doText contentText) tsText
    --  , bench "ByteString" $ nf (doByteString contentByteString) ts
    --  , bench "ByteStringUTF8" $ nf (doByteStringUTF8 contentByteString) ts
       bench "Seq Char" $ nf (doSeqChar contentSeq) tsSeq
     , bench "fold over Char" $ nf (doFoldOverChars contentText) tsString]
     ]
