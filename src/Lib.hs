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

import Prelude hiding (splitAt, length, drop, take, break, lines, foldl)
import Data.ByteString.UTF8
import qualified Data.ByteString as B hiding (lines)

-- types
data LineColumnPos = Pos Int Int
  deriving (Eq, Ord, Show)

type TokenSpan  = (LineColumnPos, LineColumnPos)

type Token  = (TokenSpan, ByteString)
type Acc = (LineColumnPos, ByteString)
type Advancement  = (Int, Int)


loadAllModules :: GHC.Ghc [([FilePath], [String])]
loadAllModules = do
  dflags <- GHC.getSessionDynFlags
  let dflags' = dflags { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink =  GHC.LinkInMemory }
      -- { GHC.importPaths = ["./test/testdata/"] }
      -- { GHC.hscTarget = GHC.HscNothing }

  GHC.setSessionDynFlags dflags'

  moduleNames <- GHC.liftIO $ concat <$> mapM getModules ["./test/stack-project/"]
  useDirs (concatMap fst moduleNames)
  GHC.setTargets $ map (\mod -> GHC.Target (GHC.TargetModule (GHC.mkModuleName mod)) True Nothing) (concatMap snd moduleNames)
  GHC.liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
  GHC.load GHC.LoadAllTargets
  return moduleNames

ghcMain :: IO ()
ghcMain =
    -- TODO send errors/exceptions/messages to client !
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.liftIO $ GHC.runGhc (Just libdir) $ do
      moduleNames <- loadAllModules
      mapM_ process (concatMap snd moduleNames)

ghcMainTest :: IO String
ghcMainTest =
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $

      GHC.liftIO $ GHC.runGhc (Just libdir) $ do
        loadAllModules
        process "TestMod"

process :: String -> GHC.Ghc String
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
        output <- GHC.liftIO $
          case file of
            Nothing  -> return ""
            Just f -> do
              content <- readFile f
              let tokens = map locTokenToPos ts
              return $ toString (loopOverForElm (fromString content) (Pos 1 1, mempty) tokens <> newline)

        GHC.liftIO $ writeFile ("./webclient/docroot/" ++ moduleName) output

        return output

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
tokenAsString :: GHC.Token -> ByteString
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
            _ -> result <> buildPart True "WS" bs

    ((pos1@(Pos l1 c1), pos2@(Pos l2 c2)), tname) : tokenTail ->
      -- putStrLn ("currentPos " ++ show currentPos ++ " pos1 " ++ show pos1 ++ " advanceLinesAndColumns " ++ show (advanceLinesAndColumns currentPos pos1)) >>

      if currentPos == pos1 then
        if pos1 == pos2 then
          loopOverForElm bs (Pos l1 c1, result <> buildPart False tname mempty) tokenTail
        else
          let advancement             = advanceLinesAndColumns pos1 pos2
              (bsHead, bsTail) = spanAdvancementElm advancement bs
          in loopOverForElm bsTail (Pos l2 c2, result <> buildPart (advancesLines advancement) tname bsHead) tokenTail
      else
        let advancement                 = advanceLinesAndColumns currentPos pos1
            (bsHead, bsTail) = spanAdvancementElm advancement bs
        in loopOverForElm bsTail (Pos l1 c1, result <> buildPart (advancesLines advancement) "WS" bsHead) tokens

  where

    buildPart :: Bool -> ByteString -> ByteString -> ByteString
    buildPart multiline token text
      | multiline = mconcat $ map (\l -> if l == newline then newline else separator <> token <> separator <> l) $ lines' text
      | text == mempty = separator <> token
      | otherwise = separator <> token <> separator <> text
      -- where
      --   txt = case token of
      --           "WS" -> foldl (\result ch -> if ch == '\n' then result <> newline else result <> wsElemDebug) mempty text
      --           _    -> text

    advanceLinesAndColumns :: LineColumnPos -> LineColumnPos -> Advancement
    advanceLinesAndColumns p1@(Pos l1 c1) p2@(Pos l2 c2)
        | p1 == p2 = (0, 0)
        | l2 - l1 > 0 = (l2 - l1, c2)
        | otherwise = (0, c2 - c1)

    advancesLines :: Advancement -> Bool
    advancesLines = (> 0) . fst

    spanAdvancementElm :: Advancement -> ByteString -> (ByteString, ByteString)
    spanAdvancementElm (l, c) bs
      | l == 0 = splitAt c bs
      | l > 0  = splitAtAdvancement (l, c - 1) bs
    spanAdvancementElm (_, _) _ = undefined

    -- LiquidHaskell: l > 0
    {-@ splitAtAdvancement :: {v:_ | fst v > 0} -> B.ByteString -> (B.ByteString, B.ByteString) @-}

    splitAtAdvancement :: Advancement -> B.ByteString -> (B.ByteString, B.ByteString)
    splitAtAdvancement (ls, cs) bs = loop 0 (ls, cs) bs
      where loop a (l, c) _ | (l, c) == (0, 0) = B.splitAt a bs
            loop a (l, c) bs1 = case decode bs1 of
                             Just (ch,y) -> case ch of
                                              '\n' -> loop (a+y) (l-1, cs) (B.drop y bs1)
                                              _    -> loop (a+y) (l, c-1) (B.drop y bs1)
                             Nothing    ->  (bs, B.empty)

    -- TODO use unicode separator here ?
    separator = fromString "⇨" -- "{}" -- "⇨"
    -- wsElemDebug = fromString "_"
    
newline = fromString "\n"
