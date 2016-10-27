module PerformanceLoopOverCharsTest (doLoopOverChars) where

import           Control.Arrow
import           Data.Sequence
import qualified Data.Text as T
import           Data.Char

import           Debug.Trace

import Lib

type Token  = (TokenSpan, String)
type Acc = (LineColumnPos, [Token], Seq Char)

decons :: T.Text -> (Char, T.Text)
decons = T.head &&& T.tail

data AdvanceMode = Column | Line

type Step = (Seq Char, [Token], Int, Int, (Char, T.Text))

advance :: Seq Char -> [Token] -> LineColumnPos -> (Char, T.Text) -> AdvanceMode -> Step
advance result tokens@(((tpos1@(Pos l1 c1), tpos2), tname) : ts) pos@(Pos l c) (ch, chs) mode =
  let (l', c') = case mode of
                  Line    -> (l + 1, 1)
                  Column  -> (l, c + 1)
  in
    -- TODO include length and/or positions of token
    -- TODO tagging after ':'  token-type, token, whitespace
    if tpos1 == pos then
      if tpos1 == tpos2 then
        (result >< ((':' <| fromList tname) |> ':'), ts, l, c, (ch, chs))
      else
        (result >< (':' <| (fromList tname |> ':' |> ch)), ts, l', c', decons chs)
    else
      (result |> ch, tokens, l', c', decons chs)


doLoopOverChars :: T.Text -> [Token] -> Seq Char
doLoopOverChars src ts =

  loopOver (Pos 1 1, ts, empty) $ decons src

  where

    loopOver :: Acc -> (Char, T.Text) -> Seq Char
    loopOver (pos@(Pos l c), tokens, result) (ch, chs) =

        let advance' = advance result tokens pos (ch, chs)
            (result', tokens', l', c', (ch', chs')) = case tokens of
                                                        []
                                                          -> (result |> ch, [], 0, 0, decons chs)

                                                        _ -> case ch of
                                                                '\n' -> advance' Line
                                                                _    -> advance' Column

      in
        if T.length chs == 0 then
          result'
        else
          loopOver (Pos l' c', tokens', result') (ch', chs')
