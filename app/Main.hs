module Main where

import Lib

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


moduleName = "LucidDemo"
targetFile = "./test/testdata/LucidDemo.hs"

main :: IO ()
main =
    -- GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do

      GHC.liftIO $ GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = dflags -- foldl GHC.xopt_set dflags [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]
            dflags'' = dflags' { GHC.importPaths = ["./test/testdata/"] }
            dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink =  GHC.LinkInMemory }

        GHC.setSessionDynFlags dflags'''

        target <- GHC.guessTarget targetFile Nothing
        GHC.setTargets [target]
        GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        GHC.liftIO (putStrLn "targets loaded")

        modSum <- GHC.getModSummary $ GHC.mkModuleName moduleName
        p <- GHC.parseModule modSum

        let ps  = GHC.pm_parsed_source p
        GHC.liftIO (putStrLn $ "ParsedSource\n\n" ++ SYB.showData SYB.Parser 0 ps)

        -- Tokens ------------------------------------------------------

        -- TODO http://ghc.haskell.org/trac/ghc/ticket/8265
        -- rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        rts <- HaRe.getRichTokenStreamWA (GHC.ms_mod modSum)

        -- let tokens_and_source =
        --      GHC.addSourceToTokens (GHC.mkRealSrcLoc (GHC.mkFastString "<file>") 1 1) (GHC.stringToStringBuffer "") ts
        -- GHC.liftIO $ putStrLn $ "addSourceToTokens=" ++ concatMap showRichToken tokens_and_source
        --

        -- GHC.liftIO (putStrLn $ concatMap showRichToken rts)

        -- send this to Elm
        GHC.liftIO (putStrLn $ "showRichTokenStream\n\n" ++ GHC.showRichTokenStream rts)
        GHC.liftIO (putStrLn $ "<EOF>\n" ++ concatMap (("\n" ++).(++ "\n").showToken) rts)


tokenLocs = map (\(GHC.L l _, s) -> (l,s))

showRichToken :: (GHC.Located GHC.Token, String) -> String
showRichToken (loc_tok, s) =
  "\n\nTOKEN " ++ tok
  ++ "\nSRC " ++ "'" ++ s ++ "'"
  ++ "\nLOC " ++ srcloc
    where
      srcloc = show $ GHC.getLoc loc_tok
      tok = show $ GHC.unLoc loc_tok

showToken :: (GHC.Located GHC.Token, String) -> String
showToken (t, s) = tok ++ "\n" ++ srcloc where
  srcloc = show $ GHC.getLoc t
  tok = show $ GHC.unLoc t

