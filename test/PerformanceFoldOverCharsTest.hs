module PerformanceFoldOverCharsTest (doFoldOverChars) where

import qualified Data.List as L (foldl')
import           Data.Sequence
import qualified Data.Text as T

import           Debug.Trace

import Lib

type Token  = (TokenSpan, String)
type Acc = ([Token], Seq Char, LineColumnPos, Maybe Seq Char)

doFoldOverChars :: T.Text -> [Token] -> Seq Char
doFoldOverChars src ts =
  let (_, src', _, _) = T.foldl' foldIt (ts, empty, Pos 1 1, Nothing) src
  in
    src'

foldIt :: Acc -> Char -> Acc
foldIt (ts, src, p@(Pos l c), accChars) ch =

  let accLength = case accChars of
                    Just ac -> length ac
                    Nothing -> 1
                             
  let (l', c') = case ch of
                   '\n' -> (l + 1, accLength)
                   _    -> (l, c + accLength)

  in
    case ts of
      [] ->
        ([], src |> ch, p)

      (((p1@(Pos l1 c1), p2@(Pos l2 c2)), t) : tokens) ->
          let token = (':' <| fromList t) |> ':'
          in
            if p1 == p then
              if p1 == p2 then
                (tokens, (src >< token), Pos l c, fmap (|> ch) accChars)
              else
                (tokens, (src >< token) |> ch, Pos l' c')
            else
              (ts, src |> ch, Pos l' c')



--      (((p1@(Pos l1 c1), p2@(Pos l2 c2)), t1) : (_, t2) : tokens) ->
--        if p1 == p then
--          let token1 = (':' <| fromList t1) |> ':'
--              token2 = fromList t2 |> ':'
--          in
--            if p1 == p2 then
--              (tokens, (src >< token1 >< token2) |> ch, Pos l' c')
--            else
--              (tail ts, (src >< token1) |> ch, Pos l' c')
--        else
--          (ts, src |> ch, Pos l' c')
