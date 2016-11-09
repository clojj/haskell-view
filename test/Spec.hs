{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Prelude hiding (readFile)
import Data.ByteString
import Lib


main :: IO ()
main = do
  src <- readFile "test/testdata/TestMod.hs"
  let expectedOutput = unlines [
                      "(*)ITmodule(*)module(*)WS(*) (*)ITconid(*)TestMod",
                      "(*)WS    (*)todo(*)((*)WS(*) (*)todo(*)someFunc",
                      "(*)WS    (*)todo(*))(*)WS(*) (*)todo(*)where",
                      "",
                      "[0:ITvocurly:[8:todo:someFunc[1:WS: [2:todo:::[1:WS: [2:ITconid:IO[1:WS: [1:todo:([1:todo:)[1:WS-:",
                      "[0:ITsemi:[8:todo:someFunc[1:WS: [1:todo:=[1:WS: [2:todo:do[3:WS-:",
                      "  [0:ITvocurly:[3:todo:let[1:WS: [0:ITvocurly:[4:todo:wahr[1:WS: [1:todo:=[1:WS: [4:todo:case[1:WS: [1:todo:([1:todo:2[1:WS: [2:todo:==[1:WS: [1:todo:1[1:WS: [1:todo:+[1:WS: [1:todo:1[1:todo:)[1:WS: [2:todo:of[16:WS-:",
                      "               [0:ITvocurly:[4:ITconid:True[1:WS: [2:todo:->[1:WS: [7:todo:\"true!\"[16:WS-:",
                      "               [0:ITsemi:[1:todo:_[4:WS:    [2:todo:->[1:WS: [5:todo:\"WTF\"[4:WS-:",
                      "",
                      "  [0:ITvccurly:[0:ITvccurly:[0:ITsemi:[8:todo:putStrLn[1:WS: [10:todo:\"someFunc\"[2:WS-:",
                      "",
                      "(*)ITblockComment(*){- A multiline comment",
                      "(*)ITblockComment(*)     which can continue for many lines",
                      "(*)ITblockComment(*)  -}",
                      "(*)ITvccurly(*)(*)ITsemi(*)(*)todo(*)multi(*)WS(*) (*)todo(*)=(*)WS(*) (*)todo(*)\"line1\\",
                      "(*)todo(*)\\line2\\",
                      "(*)todo(*)\\line3\"(*)WS(*)   (*)todo(*)-- a single line comment",
                      "",
                      "(*)ITsemi(*)"]

  hspec $

    describe "loopOverForElm" $

      it "TestMod" $ do
         output <- ghcMainTest
         output `shouldBe` expectedOutput
