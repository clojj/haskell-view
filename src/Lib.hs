module Lib ( ghcMain ) where

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
import Prelude hiding (readFile, writeFile)
import Data.Text.IO (readFile, writeFile)
import Data.Text hiding (concatMap, map, concat, foldl')
import Data.List (foldl')


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
              contents <- readFile f

              -- TODO interleave tokens with contents
              let tokens = foldl' foldToken empty ts

              writeFile ("./webclient/docroot/" ++ moduleName) $ append contents $ append (pack "<EOF>") tokens

tokenAsString :: GHC.Token -> String
tokenAsString t = case t of
  GHC.ITconid s   -> "ITconid"
  GHC.ITmodule    -> "ITmodule"
  -- TODO all tokens !
  _               -> "todo"

foldToken :: Text -> GHC.Located GHC.Token -> Text
foldToken txt locToken =
  let (GHC.RealSrcSpan loc) = GHC.getLoc locToken
      pos = (GHC.srcSpanStartLine loc, GHC.srcSpanStartCol loc)
      t   = GHC.unLoc locToken
  in append txt (pack $ show pos ++ ":" ++ tokenAsString t ++ ":")

showRichToken :: (GHC.Located GHC.Token, String) -> String
showRichToken (t, s) = tok ++ "\n" ++ srcloc where
  srcloc = show $ GHC.getLoc t
  tok = show $ GHC.unLoc t


-- useful helper functions (?)

tokenLocs = map (\(GHC.L l _, s) -> (l,s))

