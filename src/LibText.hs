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

-- Data, Typeable for GHC.Token
deriving instance Data GHC.Token
deriving instance Typeable GHC.Token

-- types
data LineColumnPos = Pos Int Int | EndLine
  deriving (Eq, Ord, Show)

type TokenSpan  = (LineColumnPos, LineColumnPos)

type Token  = (TokenSpan, T.Text)


loadAllModules :: GHC.Ghc [([FilePath], [String])]
loadAllModules = do
  dflags <- GHC.getSessionDynFlags
  let dflags' = dflags { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink =  GHC.LinkInMemory }
      -- { GHC.importPaths = ["./test/testdata/"] }
      -- { GHC.hscTarget = GHC.HscNothing }

  GHC.setSessionDynFlags dflags'

  moduleNames <- GHC.liftIO $ concat <$> mapM getModules ["../stack-project/"]
  useDirs (concatMap fst moduleNames)
  GHC.setTargets $ map (\mod -> GHC.Target (GHC.TargetModule (GHC.mkModuleName mod)) True Nothing) (concatMap snd moduleNames)
  GHC.liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
  GHC.load GHC.LoadAllTargets
  return moduleNames

-- Data.Text, new strategy

ghcMainTestSpecNew :: IO [Token]
ghcMainTestSpecNew =
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
      GHC.liftIO $ GHC.runGhc (Just libdir) $ do
                  loadAllModules
                  getAllTokens "TestMod"
      -- GHC.liftIO $ print tokens
      -- return tokens
      
getAllTokens :: String -> GHC.Ghc [Token]
getAllTokens moduleName = do
        modSum <- GHC.getModSummary (GHC.mkModuleName moduleName)

        ts <- GHC.getTokenStream (GHC.ms_mod modSum)
        let tokens_and_source = GHC.addSourceToTokens (GHC.mkRealSrcLoc (GHC.mkFastString "<file>") 1 1) (GHC.stringToStringBuffer "") ts
        GHC.liftIO $ putStrLn $ concatMap showRichToken tokens_and_source

        -- load original .hs file
        let file = GHC.ml_hs_file $ GHC.ms_location modSum
        GHC.liftIO $
          case file of
            Nothing  -> return []
            Just f -> do
              content <- readFile f
              let tokens = map locTokenToPos ts
              return tokens

-- TODO 'inline' in mapOverLines
splitMultilineTokens :: [Token] -> [Token]
splitMultilineTokens =
  concatMap splitToken 
  where
    splitToken :: Token -> [Token]
    splitToken token@((pos1@(Pos l1 c1), pos2@(Pos l2 c2)), tname)
      | l2 - l1 > 1 = [firstToken] ++ [((Pos l 1, EndLine), tname) | l <- [l1+1..l2-1]] ++ [lastToken] 
      | l2 - l1 > 0 = [firstToken, lastToken]
      | otherwise = [token]
      where
        firstToken = ((Pos l1 c1, EndLine), tname)
        lastToken  = ((Pos l2 1, Pos l2 c2), tname)

splitLineTokens :: Int -> [Token] -> ([Token], [Token])
splitLineTokens l = L.span (compareLine (l >=))

compareLine :: (Int -> Bool) -> Token -> Bool
compareLine f ((Pos l1 _, _), _) = f l1

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
                      
              ((pos1@(Pos l1 c1), pos2@(Pos l2 c2)), tname) : tsTail ->
                if col == c1 then
                  loopRecurse c2 (c2 - c1) tsTail tname
                else
                  loopRecurse c1 (c1 - col) ts "WS"

                where
                  loopRecurse nc n nextTs t = 
                    let (inTxtHead, inTxtTail) = T.splitAt n inTxt
                    in loopCol nc separator inTxtTail nextTs (outTxt <> sp <> t <> separator <> inTxtHead)

              ((Pos _ 1, EndLine), tname) : tsTail ->
                tname <> separator <> inTxt

mapOverLines :: [T.Text] -> [Token] -> [T.Text]
mapOverLines inputLines tokens =
  fst $ F.foldr' indexHelper ([], tokens) $ zip [0..] inputLines
    where
      indexHelper (i,line) (outputLines, ts)  =
        -- TODO
        ((T.pack (show i) <> " " <> line) : outputLines, ts)

                    
locTokenToPos :: GHC.Located GHC.Token -> Token
locTokenToPos locToken =
  (tokenLocToPos locToken, tokenAsString $ GHC.unLoc locToken)

tokenLocToPos :: GHC.Located GHC.Token -> TokenSpan
tokenLocToPos t =
  let (GHC.RealSrcSpan loc) = GHC.getLoc t
      [l1, c1, l2, c2] = [GHC.srcSpanStartLine, GHC.srcSpanStartCol, GHC.srcSpanEndLine, GHC.srcSpanEndCol] <*> [loc]
  in
    (Pos l1 c1, Pos l2 c2)


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

separator = "\\x001F" -- IS1 "Information Separator 1"
