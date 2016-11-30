{-# LANGUAGE OverloadedStrings #-}

import           LibText

import           Prelude      hiding (readFile)
import           Data.Text
import           Data.Text.IO
import           Data.List as L

main :: IO ()
main = do
  tokens <- ghcMainTestSpecNew
  Prelude.putStrLn $ show tokens
  