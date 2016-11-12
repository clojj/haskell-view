{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Prelude hiding (readFile)
import Data.ByteString
import Lib


main :: IO ()
main = do
  src <- readFile "test/testdata/TestMod.hs"
  let expectedOutput = unlines [
                      "⇨ITmodule⇨module⇨WS⇨ ⇨ITconid⇨TestMod",
                      "⇨WS⇨    ⇨todo⇨(⇨WS⇨ ⇨todo⇨someFunc",
                      "⇨WS⇨    ⇨todo⇨)⇨WS⇨ ⇨todo⇨where⇨WS⇨   ",
                      "⇨WS⇨ ",
                      "⇨ITvocurly⇨todo⇨someFunc⇨WS⇨ ⇨todo⇨::⇨WS⇨ ⇨ITconid⇨IO⇨WS⇨ ⇨todo⇨(⇨todo⇨)",
                      "⇨ITsemi⇨todo⇨someFunc⇨WS⇨ ⇨todo⇨=⇨WS⇨ ⇨todo⇨do",
                      "⇨WS⇨  ⇨ITvocurly⇨todo⇨let⇨WS⇨ ⇨ITvocurly⇨todo⇨wahr⇨WS⇨ ⇨todo⇨=⇨WS⇨ ⇨todo⇨case⇨WS⇨ ⇨todo⇨(⇨todo⇨2⇨WS⇨ ⇨todo⇨==⇨WS⇨ ⇨todo⇨1⇨WS⇨ ⇨todo⇨+⇨WS⇨ ⇨todo⇨1⇨todo⇨)⇨WS⇨ ⇨todo⇨of",
                      "⇨WS⇨               ⇨ITvocurly⇨ITconid⇨True⇨WS⇨ ⇨todo⇨->⇨WS⇨ ⇨todo⇨\"true!\"",
                      "⇨WS⇨               ⇨ITsemi⇨todo⇨_⇨WS⇨    ⇨todo⇨->⇨WS⇨ ⇨todo⇨\"WTF\"",
                      "",
                      "⇨WS⇨  ⇨ITvccurly⇨ITvccurly⇨ITsemi⇨todo⇨putStrLn⇨WS⇨ ⇨todo⇨\"someFunc\"",
                      "",
                      "⇨ITblockComment⇨{- A multiline comment",
                      "⇨ITblockComment⇨     which can continue for many lines",
                      "⇨ITblockComment⇨  -}",
                      "⇨ITvccurly⇨ITsemi⇨todo⇨multi⇨WS⇨ ⇨todo⇨=⇨WS⇨ ⇨todo⇨\"line1\\",
                      "⇨todo⇨\\line2\\",
                      "⇨todo⇨\\line3\"⇨WS⇨   ⇨todo⇨-- a single line comment",
                      "",
                      "⇨ITsemi"]

  hspec $

    describe "loopOverForElm" $

      it "TestMod" $ do
         output <- ghcMainTest
         output `shouldBe` expectedOutput
