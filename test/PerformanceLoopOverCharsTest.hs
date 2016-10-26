module PerformanceLoopOverCharsTest (doLoopOverChars) where

import           Control.Arrow
import           Data.Sequence
import qualified Data.Text as T
import           Data.Char

import           Debug.Trace

import Lib

type Token  = (TokenSpan, String)
type Acc = (LineColumnPos, [Token], Seq Char)

doLoopOverChars :: T.Text -> [Token] -> Seq Char
doLoopOverChars src ts =
  loopOver (Pos 1 1, ts, empty) (T.head &&& T.tail $ src)
  where

    loopOver :: Acc -> (Char, T.Text) -> Seq Char
    loopOver (pos@(Pos l c), tokens, result) (ch, chs) =

        let (result', tokens', l', c', (ch', chs')) = case tokens of
                                                        []
                                                          -> (result |> ch, [], 0, 0, T.head &&& T.tail $ chs)

                                                        ((tpos1@(Pos l1 c1), tpos2), tname) : ts
                                                          -> case ch of
                                                                '\n' ->
                                                                        if tpos1 == pos then
                                                                          (result >< (fromList tname |> ch), ts, l + 1, 1, T.head &&& T.tail $ chs)
                                                                        else
                                                                          (result |> ch, tokens, l + 1, 1, T.head &&& T.tail $ chs)
                                                                _    -> if tpos1 == pos then
                                                                          if tpos1 == tpos2 then
                                                                            (result >< fromList tname, ts, l, c, (ch, chs))
                                                                          else
                                                                            (result >< (fromList tname |> ch), ts, l, c + 1, T.head &&& T.tail $ chs)
                                                                        else
                                                                          (result |> ch, tokens, l, c + 1, T.head &&& T.tail $ chs)
      in
        if T.length chs == 0 then
          result'
        else
          loopOver (Pos l' c', tokens', result') (ch', chs')
