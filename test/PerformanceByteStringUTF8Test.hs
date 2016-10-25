{-# LANGUAGE OverloadedStrings #-}
module PerformanceByteStringUTF8Test (doByteStringUTF8) where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Monoid
import           Debug.Trace

import Lib

type Token  = (TokenSpan, UTF8.ByteString)
type Acc  = ((Int, Int), UTF8.ByteString)

f :: (Int, [Int]) -> Char -> (Int, [Int])
f (offset, xs) c = case c of
                        '\n' -> (offset + 1, offset + 1 : xs)
                        _    -> (offset + 1, xs)

doByteStringUTF8 :: UTF8.ByteString -> [Token] -> UTF8.ByteString
doByteStringUTF8 src ts =
  let ls = V.fromList $ L.reverse $ snd $ UTF8.foldl f (-1, [-1]) src
      result = L.foldl' (foldIt ls src) ((1, 0), UTF8.fromString "") ts
      lastOff = toOffset ls (fst result)
      end = UTF8.drop (lastOff + 1) src
  in
    snd result <> end

foldIt :: V.Vector Int -> UTF8.ByteString -> Acc -> Token -> Acc
foldIt ls src ((l, c), src') ((Pos l1 c1, Pos l2 c2), token) =
  let
    ws      = substr src ls (l, c) 1 (l1, c1) 0
    lexeme  = substr src ls (l1, c1) 0 (l2, c2) 1
  in
    ((l2, c2), src' <> ws <> ":" <> token <> ":" <> lexeme <> ":")

