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

  -- TODO insert (same) tokens, if token spans multiple lines
  let ldiff = l2 - l1
      ltokens = fromList $ case ldiff of
                            0 -> []
                            _ -> L.unfoldr (produceLineToken (l2 + 1)) ldiff
  in
    if l1 - l == 0 && c1 == c then
      (result |> token, pos2)
    else
      (((result |> ((Pos l c, pos1), "WS")) |> token) >< ltokens, pos2)

produceLineToken :: Int -> Int -> Maybe (Token, Int)
produceLineToken lstart l =
  case l of
    0 -> Nothing
    _ -> Just (((Pos (lstart - l) 1, Pos 0 0), "L-IT"), l - 1)