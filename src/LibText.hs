{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module LibText where

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

import Prelude hiding (splitAt, length, drop, take, break, lines, foldl)

import Data.Data
import Data.Foldable as F
import Data.List as L hiding (splitAt)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Graph (flattenSCCs)

-- Data, Typeable for GHC.Token
deriving instance Data GHC.Token
deriving instance Typeable GHC.Token

-- types
type TokenSpan  = ((Int, Int), (Int, Int))
type Token  = (TokenSpan, T.Text)


ghcMain :: IO ()
ghcMain =
    -- TODO send errors/exceptions/messages to client !
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.liftIO $ GHC.runGhc (Just libdir) $ do

      moduleNames <- loadAllModules
      GHC.liftIO $ writeFile "./webclient/docroot/.modules" $ intercalate "," (concatMap snd moduleNames)

      mapM_ process (concatMap snd moduleNames)

process :: String -> GHC.Ghc String
process moduleName = do

        modSum <- GHC.getModSummary (GHC.mkModuleName moduleName)

        -- TODO GHC.topSortModuleGraph GHC.getModuleGraph
        -- TODO only once.. it's the same each time !
        mg <- GHC.getModuleGraph
--         GHC.liftIO $ print $ GHC.showSDocUnsafe $ GHC.ppr mg
        
        let sccs = GHC.topSortModuleGraph False [modSum] Nothing
            flatSccs = flattenSCCs sccs
--         GHC.liftIO $ print $ GHC.showSDocUnsafe $ GHC.ppr sccs
        GHC.liftIO $ print $ GHC.showSDocUnsafe $ GHC.ppr flatSccs
        -- END TODO
        
        
        -- TODO use parser result
        p <- GHC.parseModule modSum
        let ps  = GHC.pm_parsed_source p
        -- GHC.liftIO (putStrLn $ "ParsedSource\n\n" ++ SYB.showData SYB.Parser 0 ps)

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
              content <- TIO.readFile f
              let tokens = map locTokenToPos ts
              return $ T.unpack . T.unlines $ mapOverLines (T.lines content) tokens

        GHC.liftIO $ writeFile ("./webclient/docroot/" ++ moduleName) output

        return output


loadAllModules :: GHC.Ghc [([FilePath], [String])]
loadAllModules = do
  dflags <- GHC.getSessionDynFlags
  let dflags' = dflags { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink =  GHC.LinkInMemory }
      -- { GHC.importPaths = ["./test/testdata/"] }
      -- { GHC.hscTarget = GHC.HscNothing }

  GHC.setSessionDynFlags dflags'

  moduleNames <- GHC.liftIO $ concat <$> mapM getModules ["../stack-project/"]
  GHC.liftIO $ putStrLn $ "moduleNames: " ++ show moduleNames
  useDirs (concatMap fst moduleNames)
  GHC.setTargets $ map (\targetModule -> GHC.Target (GHC.TargetModule (GHC.mkModuleName targetModule)) True Nothing) (concatMap snd moduleNames)
  GHC.liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
  -- TODO GHC.depanal
  GHC.load GHC.LoadAllTargets
  return moduleNames

-- START for unittest
ghcMainTestSpecNew :: IO [Token]
ghcMainTestSpecNew =
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
      GHC.liftIO $ GHC.runGhc (Just libdir) $ do
                  
                  moduleNames <- loadAllModules
                  GHC.liftIO $ do
                    print moduleNames
                    writeFile "./webclient/docroot/.modules" $ intercalate "," (concatMap snd moduleNames)
                  
                  getAllTokensForTest "TestMod"
      -- GHC.liftIO $ print tokens
      -- return tokens
      
getAllTokensForTest :: String -> GHC.Ghc [Token]
getAllTokensForTest moduleName = do
        -- TODO GHC.topSortModuleGraph GHC.getModuleGraph
  
        modSum <- GHC.getModSummary (GHC.mkModuleName moduleName)

        ts <- GHC.getTokenStream (GHC.ms_mod modSum)
        -- let tokens_and_source = GHC.addSourceToTokens (GHC.mkRealSrcLoc (GHC.mkFastString "<file>") 1 1) (GHC.stringToStringBuffer "") ts
        -- GHC.liftIO $ putStrLn $ concatMap showRichToken tokens_and_source

        -- load original .hs file
        let file = GHC.ml_hs_file $ GHC.ms_location modSum
        GHC.liftIO $
          case file of
            Nothing  -> return []
            Just f -> do
              -- TODO use in tests ?
              -- content <- readFile f
              let tokens = map locTokenToPos ts
              return tokens
              
-- END for unittest

splitMultilineTokens :: [Token] -> [Token]
splitMultilineTokens =
  concatMap splitToken 
  where
    splitToken :: Token -> [Token]
    splitToken token@(((l1, c1), (l2, c2)), tname)
      | l2 - l1 > 1 = [firstToken] ++ [(((l, 1), (0, 0)), tname) | l <- [l1+1..l2-1]] ++ [lastToken]
      | l2 - l1 > 0 = [firstToken, lastToken]
      | otherwise = [token]
      where
        firstToken = (((l1, c1), (0, 0)), tname)
        lastToken  = (((l2, 1), (l2, c2)), tname)

splitLineTokens :: Int -> [Token] -> ([Token], [Token])
splitLineTokens l = L.span (compareLine (l >=))

compareLine :: (Int -> Bool) -> Token -> Bool
compareLine f (((l1, _), _), _) = f l1

tokenizeLine :: T.Text -> [Token] -> T.Text
tokenizeLine input tokens = 
  case input of
    "" -> input
    _  -> loopCol 1 T.empty input tokens T.empty
    where loopCol col sp inTxt ts outTxt =
            case ts of
              [] -> case inTxt of
                      "" -> outTxt
                      _  -> outTxt <> sp <> "WS" <> separator <> inTxt
                      
              -- all multilines, except last
              ((_, (0, 0)), tname) : _ ->
                outTxt <> sp <> tname <> separator <> inTxt

              (((l1, c1), (l2, c2)), tname) : tsTail 
                | l1 == l2 && c1 == c2 -> loopRecurse c1 0 tsTail tname
                | col == c1            -> loopRecurse c2 (c2 - c1) tsTail tname
                | otherwise            -> loopRecurse c1 (c1 - col) ts "WS"
                
                where loopRecurse nc n nextTs t
                        = let (inTxtHead, inTxtTail) = T.splitAt n inTxt in
                            loopCol nc separator inTxtTail nextTs (outTxt <> sp <> t <> separator <> inTxtHead)


mapOverLines :: [T.Text] -> [Token] -> [T.Text]
mapOverLines inputLines tokens =
  fst $ F.foldl' indexHelper ([], ts) $ zip [1..] inputLines
    where
      ts = splitMultilineTokens tokens
      indexHelper (outputLines, nextTokens) (i, inputLine)  =
        let (tsHead, tsTail) = splitLineTokens i nextTokens
            outputLine = tokenizeLine inputLine tsHead
        in (outputLines <> [outputLine], tsTail)
                    
locTokenToPos :: GHC.Located GHC.Token -> Token
locTokenToPos locToken =
  (tokenLocToPos locToken, tokenAsString $ GHC.unLoc locToken)

tokenLocToPos :: GHC.Located GHC.Token -> TokenSpan
tokenLocToPos t =
  let (GHC.RealSrcSpan loc) = GHC.getLoc t
      [l1, c1, l2, c2] = [GHC.srcSpanStartLine, GHC.srcSpanStartCol, GHC.srcSpanEndLine, GHC.srcSpanEndCol] <*> [loc]
  in
    ((l1, c1), (l2, c2))


-- TODO map tokens to token-categories (?)
tokenAsString :: GHC.Token -> T.Text
tokenAsString t =
  T.pack $ show $ toConstr t

-- helper functions

showRichToken :: (GHC.Located GHC.Token, String) -> String
showRichToken (t, s) = "\n" ++ srcloc ++ " " ++ tok ++ " " ++ s where
  srcloc = show $ GHC.getLoc t
  tok = show $ GHC.unLoc t

tokenLocs = map (\(GHC.L l _, s) -> (l,s))

separator :: T.Text
separator = "\x001F" -- IS1 "Information Separator 1"
