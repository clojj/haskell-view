{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import           LibText

import           Prelude      hiding (readFile, unlines)
import           Data.Text
import           Data.Text.IO
import           Data.List as L

main :: IO ()
main = do
  src <- readFile "test/testdata/TestMod.hs"

  let expectedTokens = [
        (((1,1),(1,29)),"ITblockComment"),
        (((3,1),(3,7)),"ITmodule"),(((3,8),(3,15)),"ITconid"),
        (((4,5),(4,6)),"IToparen"),(((4,7),(4,15)),"ITvarid"),
        (((5,5),(5,6)),"ITcparen"),(((5,7),(5,12)),"ITwhere"),
        (((7,1),(7,1)),"ITvocurly"),(((7,1),(7,7)),"ITimport"),(((7,8),(7,11)),"ITconid"),
        (((9,1),(9,1)),"ITsemi"),(((9,1),(9,4)),"ITvarid"),(((9,5),(9,6)),"ITequal"),(((9,7),(0,0)),"ITquasiQuote"),
        (((10,1),(0,0)),"ITquasiQuote"),
        (((11,1),(0,0)),"ITquasiQuote"),
        (((12,1),(0,0)),"ITquasiQuote"),
        (((13,1),(0,0)),"ITquasiQuote"),
        (((14,1),(0,0)),"ITquasiQuote"),
        (((15,1),(15,17)),"ITquasiQuote"),
        (((17,1),(17,1)),"ITsemi"),(((17,1),(17,9)),"ITvarid"),(((17,10),(17,12)),"ITdcolon"),(((17,13),(17,15)),"ITconid"),(((17,16),(17,17)),"IToparen"),(((17,17),(17,18)),"ITcparen"),
        (((18,1),(18,1)),"ITsemi"),(((18,1),(18,9)),"ITvarid"),(((18,10),(18,11)),"ITequal"),(((18,12),(18,14)),"ITdo"),
        (((19,3),(19,3)),"ITvocurly"),(((19,3),(19,6)),"ITlet"),(((19,7),(19,7)),"ITvocurly"),(((19,7),(19,11)),"ITvarid"),(((19,12),(19,13)),"ITequal"),(((19,14),(19,18)),"ITcase"),
          (((19,19),(19,20)),"IToparen"),(((19,20),(19,21)),"ITinteger"),(((19,22),(19,24)),"ITvarsym"),(((19,25),(19,26)),"ITinteger"),(((19,27),(19,28)),"ITvarsym"),(((19,29),(19,30)),"ITinteger"),
          (((19,30),(19,31)),"ITcparen"),(((19,32),(19,34)),"ITof"),
        (((20,16),(20,16)),"ITvocurly"),(((20,16),(20,20)),"ITconid"),(((20,21),(20,23)),"ITrarrow"),(((20,24),(20,31)),"ITstring"),
        (((21,16),(21,16)),"ITsemi"),(((21,16),(21,17)),"ITunderscore"),(((21,21),(21,23)),"ITrarrow"),(((21,24),(21,29)),"ITstring"),
        (((23,3),(23,3)),"ITvccurly"),(((23,3),(23,3)),"ITvccurly"),(((23,3),(23,3)),"ITsemi"),(((23,3),(23,11)),"ITvarid"),(((23,12),(23,22)),"ITstring"),
        (((25,1),(0,0)),"ITblockComment"),
        (((26,1),(0,0)),"ITblockComment"),
        (((27,1),(27,5)),"ITblockComment"),
        (((28,1),(28,1)),"ITvccurly"),(((28,1),(28,1)),"ITsemi"),(((28,1),(28,6)),"ITvarid"),(((28,7),(28,9)),"ITdcolon"),(((28,10),(28,13)),"ITconid"),
        (((29,1),(29,1)),"ITsemi"),(((29,1),(29,6)),"ITvarid"),(((29,7),(29,8)),"ITequal"),(((29,9),(29,18)),"ITvarid"),
        (((31,1),(31,1)),"ITsemi"),(((31,1),(31,6)),"ITvarid"),(((31,7),(31,8)),"ITequal"),(((31,9),(0,0)),"ITstring"),
        (((32,1),(0,0)),"ITstring"),
        (((33,1),(33,9)),"ITstring"),(((33,12),(33,36)),"ITlineComment"),
        (((35,1),(35,1)),"ITsemi")]
  
  tokens <- ghcMainTestSpecNew
  -- Prelude.putStrLn $ show tokens
  
  hspec $

  -- plan:
  -- 1) lines src
  -- 2) for each line: alternate WS ans tokens (for that line); last WS/token does NOT terminate with \x001F

    describe "usind Data.Text, new strategy" $ do

      -- TODO replace by inline multiline-splitting
      it "1) splitMultilineTokens" $
        splitMultilineTokens tokens `shouldBe` expectedTokens


      it "2) splitLineTokens fst" $ do
        let (result, _) = splitLineTokens 1 expectedTokens
        result `shouldBe` [(((1,1),(1,29)),"ITblockComment")]

      it "2a) splitLineTokens snd" $ do
        let (_, result) = splitLineTokens 1 expectedTokens
        result `shouldBe` L.drop 1 expectedTokens

      it "2b) splitLineTokens snd empty" $ do
        let (_, result) = splitLineTokens 35 expectedTokens
        result `shouldBe` []

      it "2c) splitLineTokens snd" $ do
        let (result, _) = splitLineTokens 5 $ L.drop 5 expectedTokens
        result `shouldBe` [(((5, 5), (5, 6)),"ITcparen"),(((5, 7), (5, 12)),"ITwhere")]

      it "2d) splitLineTokens fst empty" $ do
        let (result, _) = splitLineTokens 1 $ L.drop 1 expectedTokens
        result `shouldBe` []


      it "3) tokenizeLine" $ do
        let result = tokenizeLine "module TestMod" [(((1, 1), (1, 7)),"ITmodule"),(((1, 8), (1, 15)),"ITconid")]
        result `shouldBe` "ITmodule\x001Fmodule\x001FWS\x001F \x001FITconid\x001FTestMod"

      it "3a) tokenizeLine only WS, no tokens" $ do
        let result = tokenizeLine " " []
        result `shouldBe` "WS\x001F "

      it "3b) tokenizeLine empty line" $ do
        let result = tokenizeLine "" []
        result `shouldBe` ""

      it "3c) tokenizeLine" $ do
        let result = tokenizeLine " module  TestMod " [(((1, 2), (1, 8)),"ITmodule"),(((1, 10), (1, 17)),"ITconid")]
        result `shouldBe` "WS\x001F \x001FITmodule\x001Fmodule\x001FWS\x001F  \x001FITconid\x001FTestMod\x001FWS\x001F "

      it "3d) tokenizeLine multiline-first" $ do
        let result = tokenizeLine "multi = \"line1\\" [(((31,1),(31,1)),"ITsemi"),(((31,1),(31,6)),"ITvarid"),(((31,7),(31,8)),"ITequal"),(((31,9),(0,0)),"ITstring")]
        result `shouldBe` "ITsemi\x001F\x001FITvarid\x001Fmulti\x001FWS\x001F \x001FITequal\x001F=\x001FITstring\x001F \"line1\\"

      it "3e) tokenizeLine multiline-middle" $ do
        let result = tokenizeLine "  \\ line2 \\  " [(((1, 1), (0, 0)),"ITstring")]
        result `shouldBe` "ITstring\x001F  \\ line2 \\  "


      it "4) mapOverLines" $ do
        let result = mapOverLines
                      ["module TestMod", "line1 \\", "  \\ line2 \\  ", "line3", " module  TestMod "]
                      [(((1, 1), (1, 7)),"ITmodule"), (((1, 8), (1, 15)),"ITconid"),
                       (((2, 1), (4, 6)),"ITstring"),
                       (((5, 2), (5, 8)),"ITmodule"), (((5, 10), (5, 17)),"ITconid")
                      ]
        result `shouldBe` ["ITmodule\x001Fmodule\x001FWS\x001F \x001FITconid\x001FTestMod",
                           "ITstring\x001Fline1 \\",
                           "ITstring\x001F  \\ line2 \\  ",
                           "ITstring\x001Fline3",
                           "WS\x001F \x001FITmodule\x001Fmodule\x001FWS\x001F  \x001FITconid\x001FTestMod\x001FWS\x001F "
                          ]

