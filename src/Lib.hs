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
import Prelude hiding (readFile, writeFile, take, drop)
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import Data.Text hiding (concatMap, map, concat, foldl')
import Data.String
import Data.List (foldl')
import Data.Monoid

import Data.Vector ((!), Vector)
import qualified Data.ByteString.Char8 as BSC

-- types
data LineColumnPos = Pos Int Int
  deriving (Eq, Ord, Show)

type TokenSpan  = (LineColumnPos, LineColumnPos)



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

              -- TODO interleave tokens with content
              let lines = splitOn "\n" content
              let tokenizedSource = foldl' (foldToken lines) empty ts

              writeFile ("./webclient/docroot/" ++ moduleName) $ content <> "<EOF>\n" <> "[" <> tokenizedSource <> "]"

-- TODO replace with code from PerformanceTest
foldToken :: [Text] -> Text -> GHC.Located GHC.Token -> Text
foldToken lines result locToken =
  -- let (GHC.RealSrcSpan loc) = GHC.getLoc locToken
  --     [l1, c1, l2, c2] = fmap (1 -) $ [GHC.srcSpanStartLine, GHC.srcSpanStartCol, GHC.srcSpanEndLine, GHC.srcSpanEndCol] <*> [loc]
      -- TODO need the position in client ?
      -- position = show fromLine ++ " " ++ show fromCol ++ " " ++ show toLine ++ " "++ show toCol ++ " "

  result <> "(" <> showTokenSpan locToken <> ", " <> "\"" <> tokenAsString (GHC.unLoc locToken) <> "\"" <> ")," <> "\n"

-- TODO map tokens to class-names
tokenAsString :: GHC.Token -> Text
tokenAsString t = case t of
  GHC.ITconid s         -> "ITconid"
  GHC.ITmodule          -> "ITmodule"
  GHC.ITblockComment s  -> "ITblockComment"

  GHC.ITocurly          -> "special"
  GHC.ITccurly          -> "special"
  GHC.ITvocurly         -> "special"
  GHC.ITvccurly         -> "special"
  -- TODO all tokens !
  -- _               -> T.pack $ show t
  _               -> "todo"

-- helper functions

toOffset :: Vector Int -> (Int, Int) -> Int
toOffset ls (l, c) =
  (ls ! (l-1)) + c

substr :: BSC.ByteString -> Vector Int -> (Int, Int) -> Int -> (Int, Int) -> Int -> BSC.ByteString
substr src ls (l1, c1) o1 (l2, c2) o2 =
  let
    off1 = (ls ! (l1-1)) + c1 + o1
    off2 = (ls ! (l2-1)) + c2 + o2
    len  = off2 - off1
  in (BSC.take len . BSC.drop off1) src

substrText :: Text -> Vector Int -> (Int, Int) -> Int -> (Int, Int) -> Int -> Text
substrText src ls (l1, c1) o1 (l2, c2) o2 =
  let
    off1 = (ls ! (l1-1)) + c1 + o1
    off2 = (ls ! (l2-1)) + c2 + o2
    len  = off2 - off1
  in (take len . drop off1) src

showTokenSpan :: GHC.Located GHC.Token -> Text
showTokenSpan t =
  let (GHC.RealSrcSpan loc) = GHC.getLoc t
      [l1, c1, l2, c2] = [GHC.srcSpanStartLine, GHC.srcSpanStartCol, GHC.srcSpanEndLine, GHC.srcSpanEndCol] <*> [loc]
  in
    T.pack $ show (Pos l1 c1, Pos l2 c2)

showRichToken :: (GHC.Located GHC.Token, String) -> String
showRichToken (t, s) = tok ++ "\n" ++ srcloc where
  srcloc = show $ GHC.getLoc t
  tok = show $ GHC.unLoc t

tokenLocs = map (\(GHC.L l _, s) -> (l,s))
