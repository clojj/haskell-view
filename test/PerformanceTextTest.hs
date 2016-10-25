{-# LANGUAGE OverloadedStrings #-}
module PerformanceTextTest (doText) where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Monoid
import           Debug.Trace

import Lib

type Token  = (TokenSpan, T.Text)
type Acc  = ((Int, Int), T.Text)

f :: (Int, [Int]) -> Char -> (Int, [Int])
f (offset, xs) c = case c of
                        '\n' -> (offset + 1, offset + 1 : xs)
                        _    -> (offset + 1, xs)

doText :: T.Text -> [Token] -> T.Text
doText src ts =
  let ls = V.fromList $ L.reverse $ snd $ T.foldl' f (-1, [-1]) src
      result = L.foldl' (foldText ls src) ((1, 1), T.empty) ts
      lastOff = toOffset ls (fst result)
      end = T.drop (lastOff + 1) src
  in
    snd result <> end

foldText :: V.Vector Int -> T.Text -> Acc -> Token -> Acc
foldText ls src ((l, c), src') ((Pos l1 c1, Pos l2 c2), token) =
  let
    ws      = substrText src ls (l, c) 1 (l1, c1) 0
    lexeme  = substrText src ls (l1, c1) 0 (l2, c2) 1
  in
    -- ((l2, c2), (T.append (T.append (T.append (T.append (T.append (T.append src' ws) ":") token) ":") lexeme) ":"))
    ((l2, c2), src' <> ws <> ":" <> token <> ":" <> lexeme <> ":")

