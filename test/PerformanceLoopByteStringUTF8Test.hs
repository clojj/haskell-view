{-# LANGUAGE OverloadedStrings #-}
module PerformanceLoopByteStringUTF8Test (loopOverForElm) where

import Prelude hiding (splitAt, length, foldl, break, drop, take, span)
import Data.ByteString.UTF8 hiding (foldr)
import qualified Data.ByteString as B
import Data.Monoid

import Lib
import Tokens

-- import Debug.Trace

type Token  = (TokenSpan, String)
type Acc = (LineColumnPos, ByteString)

type Advancement  = (Int, Int)

-- TODO type Line = Int
-- TODO type Column = Int
-- TODO type Offset = Int


advanceLinesAndColumns :: LineColumnPos -> LineColumnPos -> Advancement
advanceLinesAndColumns p1@(Pos l1 c1) p2@(Pos l2 c2)
    | p1 == p2 = (0, 0)
    | l2 - l1 > 0 = (l2 - l1, c2)
    | otherwise = (0, c2 - c1)

loopOverForElm :: ByteString -> Acc -> [Token] -> ByteString
loopOverForElm bs (currentPos, result) tokens =
  case tokens of

    [] -> result <> sepElmStart <> fromString (show $ length bs) <> fromString ":WS" <> sepElmEnd <> bs

    ((pos1@(Pos l1 c1), pos2@(Pos l2 c2)), tname) : tokenTail ->
      -- putStrLn ("currentPos " ++ show currentPos ++ " pos1 " ++ show pos1 ++ " advanceLinesAndColumns " ++ show (advanceLinesAndColumns currentPos pos1)) >>

      if currentPos == pos1 then
        if pos1 == pos2 then
          loopOverForElm bs (Pos l1 c1, result <> sepElmStart <> fromString tname <> sepElmEnd) tokenTail
        else
          let advancement     = advanceLinesAndColumns pos1 pos2
              (len, (token, bsTail)) = spanAdvancementElm advancement bs
              -- (token, bsTail) = spanLines advancement bs
          in loopOverForElm bsTail (Pos l2 c2, result <> sepElmStart <> fromString (show len) <> fromString (":" ++ tname) <> sepElmEnd <> token) tokenTail
      else
        let advancement  = advanceLinesAndColumns currentPos pos1
            (len, (ws, bsTail)) = spanAdvancementElm advancement bs
            -- (ws, bsTail) = spanLines advancement bs
        in loopOverForElm bsTail (Pos l1 c1, result <> sepElmStart <> fromString (show len) <> fromString ":WS" <> sepElmEnd <> ws) tokens

sepElmStart :: ByteString
sepElmStart = fromString "["

sepElmEnd :: ByteString
sepElmEnd = fromString "]"

spanAdvancementElm :: Advancement -> ByteString -> (Int, (ByteString, ByteString))
spanAdvancementElm (l, c) bs
  | l == 0 = (c, splitAt c bs)
  -- | l > 0  = splitAt ((lineOffset l bs) + c - 1) bs
  | l > 0  = splitAtElm (l, c - 1) bs
spanAdvancementElm (_, _) _ = undefined

-- LiquidHaskell: l > 0
{-@ splitAtElm :: {v:_ | fst v > 0} -> B.ByteString -> (Int, (B.ByteString, B.ByteString)) @-}

splitAtElm :: Advancement -> B.ByteString -> (Int, (B.ByteString, B.ByteString))
splitAtElm (ls, cs) bs = loop 0 (ls, cs) bs
  where loop a (l, c) _ | (l, c) == (0, 0) = (a, B.splitAt a bs)
        loop a (l, c) bs1 = case decode bs1 of
                         Just (ch,y) -> case ch of
                                          '\n' -> loop (a+y) (l-1, cs) (B.drop y bs1)
                                          _    -> loop (a+y) (l, c-1) (B.drop y bs1)
                         Nothing    ->  (0, (bs, B.empty))

