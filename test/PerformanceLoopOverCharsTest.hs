module PerformanceLoopOverCharsTest (doLoopOverChars) where

import           Control.Arrow
import           Data.Sequence
import qualified Data.Text as T
import qualified Data.Foldable as F
import           Data.Char
import           Data.List as L

import           Debug.Trace

import Lib
import Tokens


type Token  = (TokenSpan, String)
type Acc = (LineColumnPos, [Token], Seq Char)

decons :: T.Text -> (Char, T.Text)
decons = T.head &&& T.tail

data AdvanceMode = Column | Line

type Step = (Seq Char, [Token], Int, Int, (Char, T.Text))

doLoopOverChars :: T.Text -> [Token] -> Seq Char
doLoopOverChars src ts =

  loopOver (Pos 1 1, F.toList $ insertWhitespace ts, empty) $ decons src

  where

    loopOver :: Acc -> (Char, T.Text) -> Seq Char
    loopOver (pos@(Pos l c), tokens, result) (ch, chs) =

        let (result', tokens', l', c', (ch', chs')) = case tokens of
                                                        [] -> ((result |> ch) >< fromText chs, [], 0, 0, (ch, chs))

                                                        _  -> let adv = advance result tokens pos (ch, chs)
                                                              in case ch of
                                                                   '\n' -> adv Line
                                                                   _    -> adv Column

      in
        if T.length chs == 0 then
          result'
        else
          loopOver (Pos l' c', tokens', result') (ch', chs')

advance :: Seq Char -> [Token] -> LineColumnPos -> (Char, T.Text) -> AdvanceMode -> Step
advance result tokens@(((tpos1@(Pos l1 c1), tpos2), tname) : ts) pos@(Pos l c) (ch, chs) mode =

  -- TODO after start of token (at tpos1), immediately advance to tpos2
  -- is this really faster ?

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

-- is this really faster ?
fromText :: T.Text -> Seq Char
fromText txt = fromList $ T.unpack txt

