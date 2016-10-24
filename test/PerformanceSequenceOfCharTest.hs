module PerformanceSequenceOfCharTest (doSeqChar) where

import           Prelude hiding (splitAt, length)
import           Data.List (foldl')
import qualified Data.Vector as V
import           Data.Sequence as S hiding (reverse)

import           Debug.Trace

import Lib
import TestHelpers

-- TODO
-- 1) elemIndicesL
-- 2) insertAt

type Token  = (Located, Seq Char)
-- type Acc = (Seq Char, Int, V.Vector Int, Int)
type Acc = (Seq Char, Int, V.Vector Int, Int, Int)

doSeqChar :: Seq Char -> [Token] -> Seq Char
doSeqChar src ts = {-# SCC "doSeqChar" #-}
  let ls = V.fromList $ -1 : elemIndicesL '\n' src
      -- lsTraced = trace ("ls: " ++ show ls) ls
      -- (src', _, _, _, _) = foldl' foldSeq (src, 1, lsTraced, 0) $ reverse ts
      (src', _, _, _, _) = foldl' foldSeqChar (src, 1, ls, 0, 0) $ reverse ts
  in
    src'

foldSeqChar :: Acc -> Token -> Acc
foldSeqChar (src, l, ls, cOff, aOff) (((l1, c1), _), token) =
  let
    (l', cOff') = if l == l1 then (l, cOff) else (l1, 0)
    insertLen = length token + 2
    aOff' = aOff + insertLen

    off = ls V.! (l1-1) + c1 + cOff
    (src1, src2) = S.splitAt off src
  in
    (src1 >< token >< src2, l', ls, cOff', aOff')

