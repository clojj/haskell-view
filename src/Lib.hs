{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified GHC.SYB.Utils         as SYB

import qualified DynFlags              as GHC
import qualified ErrUtils              as GHC
import qualified Exception             as GHC
import qualified FastString            as GHC
import qualified GHC
import qualified HscTypes              as GHC
import qualified Lexer                 as GHC
import qualified MonadUtils            as GHC
import qualified Outputable            as GHC
import qualified SrcLoc                as GHC
import qualified StringBuffer          as GHC

import GHC.Paths ( libdir )

import qualified Language.Haskell.Refact.Utils.GhcBugWorkArounds as HaRe

import GetModules
import Data.Monoid

import qualified Data.ByteString.Char8 as BSC

import Prelude hiding (splitAt, length, drop, take)
import Data.ByteString.UTF8
import qualified Data.ByteString as B

-- types
data LineColumnPos = Pos Int Int
  deriving (Eq, Ord, Show)

type TokenSpan  = (LineColumnPos, LineColumnPos)

type Token  = (TokenSpan, String)
type Acc = (LineColumnPos, ByteString)
type Advancement  = (Int, Int)


ghcMain :: IO ()
ghcMain =
    -- TODO send errors/exceptions/messages to client !
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $

      GHC.liftIO $ GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = dflags { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink =  GHC.LinkInMemory }
            -- { GHC.importPaths = ["./test/testdata/"] }
            -- { GHC.hscTarget = GHC.HscNothing }
            dflags'' = dflags'

        GHC.setSessionDynFlags dflags''

        moduleNames <- GHC.liftIO $ concat <$> mapM getModules ["./test/stack-project/"]
        useDirs (concatMap fst moduleNames)
        GHC.setTargets $ map (\mod -> GHC.Target (GHC.TargetModule (GHC.mkModuleName mod)) True Nothing) (concatMap snd moduleNames)
        GHC.liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
        GHC.load GHC.LoadAllTargets

        mapM_ process (concatMap snd moduleNames)

process :: String -> GHC.Ghc ()
process moduleName = do
        modSum <- GHC.getModSummary (GHC.mkModuleName moduleName)

        -- TODO use parser result
        p <- GHC.parseModule modSum
        let ps  = GHC.pm_parsed_source p
        GHC.liftIO (putStrLn $ "ParsedSource\n\n" ++ SYB.showData SYB.Parser 0 ps)

        -- Tokens ------------------------------------------------------

        -- TODO check if fixed ? http://ghc.haskell.org/trac/ghc/ticket/8265
        -- rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        -- rts <- HaRe.getRichTokenStreamWA (GHC.ms_mod modSum)
        ts <- GHC.getTokenStream (GHC.ms_mod modSum)

        -- let tokens_and_source = GHC.addSourceToTokens (GHC.mkRealSrcLoc (GHC.mkFastString "<file>") 1 1) (GHC.stringToStringBuffer "") ts
        -- GHC.liftIO $ putStrLn $ "addSourceToTokens=" ++ concatMap showRichToken tokens_and_source

        -- TODO sourceId: filename, path, module-name, ...?
        let sourceId = moduleName

        -- let tokens = concatMap (("\n" ++).(++ "\n").showRichToken) rts
        -- let sourceAndTokens = sourceId ++ "\n" ++ GHC.showRichTokenStream rts ++ "<EOF>\n" ++ tokens
        -- GHC.liftIO (putStrLn sourceAndTokens)
        -- GHC.liftIO $ writeFile ("./webclient/docroot/" ++ moduleName) sourceAndTokens

        -- load original .hs file
        let file = GHC.ml_hs_file $ GHC.ms_location modSum
        GHC.liftIO $
          case file of
            Nothing  -> return ()
            Just f -> do
              content <- readFile f
              let tokens = map locTokenToPos ts
              writeFile ("./webclient/docroot/" ++ moduleName) $ toString $ loopOverForElm (fromString content) (Pos 1 1, mempty) tokens

locTokenToPos :: GHC.Located GHC.Token -> Token
locTokenToPos locToken =
  (tokenLocToPos locToken, tokenAsString $ GHC.unLoc locToken)

tokenLocToPos :: GHC.Located GHC.Token -> TokenSpan
tokenLocToPos t =
  let (GHC.RealSrcSpan loc) = GHC.getLoc t
      [l1, c1, l2, c2] = [GHC.srcSpanStartLine, GHC.srcSpanStartCol, GHC.srcSpanEndLine, GHC.srcSpanEndCol] <*> [loc]
  in
    (Pos l1 c1, Pos l2 c2)


-- TODO map tokens to class-names
tokenAsString :: GHC.Token -> String
tokenAsString t = case t of
  GHC.ITconid s         -> "ITconid"
  GHC.ITmodule          -> "ITmodule"
  GHC.ITblockComment s  -> "ITblockComment"

  GHC.ITocurly          -> "ITocurly"
  GHC.ITccurly          -> "ITccurly"
  GHC.ITvocurly         -> "ITvocurly"
  GHC.ITvccurly         -> "ITvccurly"
  GHC.ITsemi            -> "ITsemi"

  -- TODO all tokens !
  -- _               -> T.pack $ show t
  _               -> "todo"


-- helper functions

showRichToken :: (GHC.Located GHC.Token, String) -> String
showRichToken (t, s) = tok ++ "\n" ++ srcloc where
  srcloc = show $ GHC.getLoc t
  tok = show $ GHC.unLoc t

tokenLocs = map (\(GHC.L l _, s) -> (l,s))

--

loopOverForElm :: ByteString -> Acc -> [Token] -> ByteString
loopOverForElm bs (currentPos, result) tokens =
  case tokens of

    [] -> case length bs of
            0 -> result
            _ -> result <> buildPart (length bs) True "WS" bs

    ((pos1@(Pos l1 c1), pos2@(Pos l2 c2)), tname) : tokenTail ->
      -- putStrLn ("currentPos " ++ show currentPos ++ " pos1 " ++ show pos1 ++ " advanceLinesAndColumns " ++ show (advanceLinesAndColumns currentPos pos1)) >>

      if currentPos == pos1 then
        if pos1 == pos2 then
          loopOverForElm bs (Pos l1 c1, result <> buildPart 0 False tname mempty) tokenTail
        else
          let advancement             = advanceLinesAndColumns pos1 pos2
              (len, (lexeme, bsTail)) = spanAdvancementElm advancement bs
          in loopOverForElm bsTail (Pos l2 c2, result <> buildPart len (advancesLines advancement) tname lexeme) tokenTail
      else
        let advancement                 = advanceLinesAndColumns currentPos pos1
            (len, (whitespace, bsTail)) = spanAdvancementElm advancement bs
        in loopOverForElm bsTail (Pos l1 c1, result <> buildPart len (advancesLines advancement) "WS" whitespace) tokens

  where

    buildPart :: Int -> Bool -> String -> ByteString -> ByteString
    buildPart len multiline element text =
      sepElmStart <> fromString (show len) <> separator <> fromString element <> (if multiline then postFixMultiline else mempty) <> separator <> text <> mempty

    advanceLinesAndColumns :: LineColumnPos -> LineColumnPos -> Advancement
    advanceLinesAndColumns p1@(Pos l1 c1) p2@(Pos l2 c2)
        | p1 == p2 = (0, 0)
        | l2 - l1 > 0 = (l2 - l1, c2)
        | otherwise = (0, c2 - c1)

    advancesLines :: Advancement -> Bool
    advancesLines = (> 0) . fst

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

    separator = fromString ":"
    postFixMultiline = fromString "-"
    sepElmStart = fromString "["
    -- sepElmEnd    = fromString "]"
