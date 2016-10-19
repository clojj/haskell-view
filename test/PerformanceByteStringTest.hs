{-# LANGUAGE OverloadedStrings #-}
module PerformanceByteStringTest (doByteString) where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C
import           Data.Monoid
import           Debug.Trace

import Lib
import TestHelpers

type Token  = (Located, C.ByteString)
type Acc  = ((Int, Int), C.ByteString)

doByteString :: C.ByteString -> [Token] -> C.ByteString
doByteString src ts =
  let ls = V.fromList $ -1 : C.elemIndices '\n' src
      result = L.foldl' (foldByteString ls src) ((1, 0), C.empty) ts
      lastOff = toOffset ls (fst result)
      end = C.drop (lastOff + 1) src
  in
    snd result <> end

foldByteString :: V.Vector Int -> C.ByteString -> Acc -> Token -> Acc
foldByteString ls src ((l, c), src') (((l1, c1), (l2, c2)), token) =
  let
    ws      = substr src ls (l, c) 1 (l1, c1) 0
    lexeme  = substr src ls (l1, c1) 0 (l2, c2) 1
  in
    ((l2, c2), src' <> ws <> ":" <> token <> ":" <> lexeme <> ":")

