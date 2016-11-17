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

  let expectedTokens = [((Pos 1 1,Pos 1 7),"ITmodule"),((Pos 1 8,Pos 1 15),"ITconid"),
                        ((Pos 2 5,Pos 2 6),"IToparen"),((Pos 2 7,Pos 2 15),"ITvarid"),
                        ((Pos 3 5,Pos 3 6),"ITcparen"),((Pos 3 7,Pos 3 12),"ITwhere"),
                        ((Pos 5 1,Pos 5 1),"ITvocurly"),((Pos 5 1,Pos 5 9),"ITvarid"),((Pos 5 10,Pos 5 12),"ITdcolon"),((Pos 5 13,Pos 5 15),"ITconid"),((Pos 5 16,Pos 5 17),"IToparen"),((Pos 5 17,Pos 5 18),"ITcparen"),
                        ((Pos 6 1,Pos 6 1),"ITsemi"),((Pos 6 1,Pos 6 9),"ITvarid"),((Pos 6 10,Pos 6 11),"ITequal"),((Pos 6 12,Pos 6 14),"ITdo"),
                        ((Pos 7 3,Pos 7 3),"ITvocurly"),((Pos 7 3,Pos 7 6),"ITlet"),((Pos 7 7,Pos 7 7),"ITvocurly"),((Pos 7 7,Pos 7 11),"ITvarid"),((Pos 7 12,Pos 7 13),"ITequal"),((Pos 7 14,Pos 7 18),"ITcase"),((Pos 7 19,Pos 7 20),"IToparen"),((Pos 7 20,Pos 7 21),"ITinteger"),((Pos 7 22,Pos 7 24),"ITvarsym"),((Pos 7 25,Pos 7 26),"ITinteger"),((Pos 7 27,Pos 7 28),"ITvarsym"),((Pos 7 29,Pos 7 30),"ITinteger"),((Pos 7 30,Pos 7 31),"ITcparen"),((Pos 7 32,Pos 7 34),"ITof"),
                        ((Pos 8 16,Pos 8 16),"ITvocurly"),((Pos 8 16,Pos 8 20),"ITconid"),((Pos 8 21,Pos 8 23),"ITrarrow"),((Pos 8 24,Pos 8 31),"ITstring"),
                        ((Pos 9 16,Pos 9 16),"ITsemi"),((Pos 9 16,Pos 9 17),"ITunderscore"),((Pos 9 21,Pos 9 23),"ITrarrow"),((Pos 9 24,Pos 9 29),"ITstring"),
                        ((Pos 11 3,Pos 11 3),"ITvccurly"),((Pos 11 3,Pos 11 3),"ITvccurly"),((Pos 11 3,Pos 11 3),"ITsemi"),((Pos 11 3,Pos 11 11),"ITvarid"),((Pos 11 12,Pos 11 22),"ITstring"),
                        ((Pos 13 1,EndLine),"ITblockComment"),
                        ((Pos 14 1,EndLine),"ITblockComment"),
                        ((Pos 15 1,Pos 15 5),"ITblockComment"),
                        ((Pos 16 1,Pos 16 1),"ITvccurly"),((Pos 16 1,Pos 16 1),"ITsemi"),((Pos 16 1,Pos 16 6),"ITvarid"),((Pos 16 7,Pos 16 9),"ITdcolon"),((Pos 16 10,Pos 16 13),"ITconid"),
                        ((Pos 17 1,Pos 17 1),"ITsemi"),((Pos 17 1,Pos 17 6),"ITvarid"),((Pos 17 7,Pos 17 8),"ITequal"),((Pos 17 9,Pos 17 18),"ITvarid"),
                        ((Pos 19 1,Pos 19 1),"ITsemi"),((Pos 19 1,Pos 19 6),"ITvarid"),((Pos 19 7,Pos 19 8),"ITequal"),((Pos 19 9,EndLine),"ITstring"),
                        ((Pos 20 1,EndLine),"ITstring"),
                        ((Pos 21 1,Pos 21 8),"ITstring"),((Pos 21 11,Pos 21 35),"ITlineComment"),
                        ((Pos 23 1,Pos 23 1),"ITsemi")]
  
  tokens <- ghcMainTestSpecNew
  
  hspec $

  -- plan:
  -- 1) lines src
  -- 2) for each line: alternate WS ans tokens (for that line); last WS/token does NOT terminate with \\x001F

    describe "usind Data.Text, new strategy" $ do

      it "1) splitMultilineTokens" $
        splitMultilineTokens tokens `shouldBe` expectedTokens


      it "2) splitLineTokens fst" $ do
        let (result, _) = splitLineTokens 1 expectedTokens
        result `shouldBe` [((Pos 1 1,Pos 1 7),"ITmodule"),((Pos 1 8,Pos 1 15),"ITconid")]

      it "2a) splitLineTokens snd" $ do
        let (_, result) = splitLineTokens 1 expectedTokens
        result `shouldBe` L.drop 2 expectedTokens

      it "2b) splitLineTokens snd empty" $ do
        let (_, result) = splitLineTokens 23 expectedTokens
        result `shouldBe` []

      it "2c) splitLineTokens snd" $ do
        let (result, _) = splitLineTokens 3 $ L.drop 4 expectedTokens
        result `shouldBe` [((Pos 3 5,Pos 3 6),"ITcparen"),((Pos 3 7,Pos 3 12),"ITwhere")]

      it "2d) splitLineTokens fst empty" $ do
        let (result, _) = splitLineTokens 1 $ L.drop 2 expectedTokens
        result `shouldBe` []


      it "3) tokenizeLine" $ do
        let result = tokenizeLine "module TestMod" [((Pos 1 1,Pos 1 7),"ITmodule"),((Pos 1 8,Pos 1 15),"ITconid")]
        result `shouldBe` "ITmodule\\x001Fmodule\\x001FWS\\x001F \\x001FITconid\\x001FTestMod"

      it "3a) tokenizeLine only WS, no tokens" $ do
        let result = tokenizeLine " " []
        result `shouldBe` "WS\\x001F "

      it "3b) tokenizeLine empty line" $ do
        let result = tokenizeLine "" []
        result `shouldBe` ""

      it "3c) tokenizeLine" $ do
        let result = tokenizeLine " module  TestMod " [((Pos 1 2,Pos 1 8),"ITmodule"),((Pos 1 10,Pos 1 17),"ITconid")]
        result `shouldBe` "WS\\x001F \\x001FITmodule\\x001Fmodule\\x001FWS\\x001F  \\x001FITconid\\x001FTestMod\\x001FWS\\x001F "


