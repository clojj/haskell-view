module PerformanceFoldOverCharsTest (doFoldOverChars) where

import qualified Data.List as L (foldl')
import           Data.Sequence
import qualified Data.Text as T

import           Debug.Trace

import Lib
import TestHelpers

type Token  = (Located, String)
type Acc = ([Token], Seq Char, Int, Int)

doFoldOverChars :: T.Text -> [Token] -> Seq Char
doFoldOverChars src ts =
  let
    (_, src', _, _) = T.foldl' f (ts, empty, 1, 1) src
  in
    src'

f :: Acc -> Char -> Acc
f (ts, src, l, c) ch =
  let (l', c') = case ch of
                   '\n' -> (l + 1, 1)
                   _    -> (l, c + 1)
  in
    case ts of
      [] ->
        ([], src |> ch, l, c)

      ((((l1, c1), (l2, c2)), t1) : (loc2, t2) : tokens) ->
        if l1 == l && c1 == c then
            if l1 == l2 && c1 == c2 then
              (tokens, (src |> ':') >< (fromList t1 |> ':') >< (fromList t2 |> ':' |> ch), l', c')
            else
              ((loc2, t2) : tokens, (src |> ':') >< (fromList t1 |> ':' |> ch), l', c')
        else
          ((((l1, c1), (l2, c2)), t1) : (loc2, t2) : tokens, src |> ch, l', c')

      ((((l1, c1), (l2, c2)), t1) : tokens) ->
        if l1 == l && c1 == c then
          (tokens, (src |> ':') >< (fromList t1 |> ':' |> ch), l', c')
        else
          ((((l1, c1), (l2, c2)), t1) : tokens, src |> ch, l', c')

