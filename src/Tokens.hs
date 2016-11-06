module Tokens (insertWhitespace) where

import           Data.Sequence
import qualified Data.Text as T
import           Data.Char
import           Data.List as L

import           Debug.Trace

import Lib

type Token  = (TokenSpan, String)
type TAcc = (Seq Token, LineColumnPos)

-- prepare [Token] by inserting WS tokens

insertWhitespace :: [Token] -> Seq Token
insertWhitespace = fst . L.foldl' decideToken (empty, Pos 1 1)

decideToken :: TAcc -> Token -> TAcc
decideToken (result, prevPos@(Pos l c)) token@((pos1@(Pos l1 c1), pos2@(Pos l2 c2)), tname) =

  if l1 - l == 0 && c1 == c then
    -- virtual tokens (semi, vocurly etc)
    (result |> token, pos2)
  else
    let ldiff = l2 - l1
    in
      let s = result |> ((Pos l c, pos1), "WS") |> token
      in
        case ldiff of
          0 -> (s, pos2)
          _ -> (s >< fromList (L.unfoldr (produceLineToken token (l2 + 1)) ldiff), pos2)

-- TODO 'Pos 0 0' currently used as marker for "ends at next token"
produceLineToken :: Token -> Int -> Int -> Maybe (Token, Int)
produceLineToken (_, tname) lstart l =
  case l of
    0 -> Nothing
    _ -> Just (((Pos (lstart - l) 1, Pos 0 0), tname), l - 1)
