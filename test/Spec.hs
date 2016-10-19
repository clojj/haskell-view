{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Prelude hiding (readFile, take, drop)
import Data.ByteString.Char8
import Data.Vector (Vector, (!), fromList)
import Lib

main :: IO ()
main = do
  src <- readFile "test/testdata/TestMod.hs"
  let ls = fromList $ -1 : elemIndices '\n' src

  hspec $ do
    describe "ByteString helpers..." $ do

      it "substr (1,1) (1,15)" $
         substr src ls (1,1) 0 (1,15) 1 `shouldBe` "module TestMod\n"

      it "substr (1,1) (3,5)" $
         substr src ls (1,1) 0 (3,5) 1 `shouldBe` "module TestMod\n    ( someFunc\n    )"

      it "substr (1,3) (3,5)" $
         substr src ls (1,3) 0 (3,5) 1 `shouldBe` "dule TestMod\n    ( someFunc\n    )"

      it "substr (1,14) (2,5)" $
         substr src ls (1,14) 0 (2,5) 1 `shouldBe` "d\n    ("

      it "substr (1,15) (2,5)" $
         substr src ls (1,15) 0 (2,5) 1 `shouldBe` "\n    ("

      it "substr (13,11) (13,34)" $
         substr src ls (13,11) 0 (13,34) 1 `shouldBe` "-- a single line comment"
