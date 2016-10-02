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


moduleName = "TestMod"
targetFile = "./test/testdata/TestMod.hs"

main :: IO ()
main =
    -- GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do

      GHC.liftIO $ GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = dflags -- foldl GHC.xopt_set dflags [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]
            dflags'' = dflags' { GHC.importPaths = ["./test/testdata/","../test/testdata/"] }
            dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink =  GHC.LinkInMemory }

        GHC.setSessionDynFlags dflags'''

        target <- GHC.guessTarget targetFile Nothing
        GHC.setTargets [target]
        GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        GHC.liftIO $ putStrLn "targets loaded"
        modSum <- GHC.getModSummary $ GHC.mkModuleName moduleName
        GHC.liftIO $ putStrLn "got modsummary"

        p <- GHC.parseModule modSum
        GHC.liftIO $ putStrLn "parsed"

        let ps  = GHC.pm_parsed_source p
        GHC.liftIO $ putStrLn "got parsed source"
        GHC.liftIO (putStrLn $ SYB.showData SYB.Parser 0 ps)

        -- Tokens ------------------------------------------------------

        -- TODO http://ghc.haskell.org/trac/ghc/ticket/8265
        -- rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        rts <- HaRe.getRichTokenStreamWA (GHC.ms_mod modSum)

        -- let tokens_and_source =
        --      GHC.addSourceToTokens (GHC.mkRealSrcLoc (GHC.mkFastString "<file>") 1 1) (GHC.stringToStringBuffer "") ts
        -- GHC.liftIO $ putStrLn $ "addSourceToTokens=" ++ concatMap showRichToken tokens_and_source
        --

        GHC.liftIO (putStrLn $ "showRichTokenStream=" ++ GHC.showRichTokenStream rts)
        GHC.liftIO $ putStrLn $ concatMap showRichToken rts


tokenLocs = map (\(GHC.L l _, s) -> (l,s))

showRichToken :: (GHC.Located GHC.Token, String) -> String
showRichToken (loc_tok, s) =
  "\n\nTOKEN " ++ tok
  ++ "\nSRC " ++ "'" ++ s ++ "'"
  ++ "\nLOC " ++ srcloc
    where
      srcloc = show $ GHC.getLoc loc_tok
      tok = show $ GHC.unLoc loc_tok

showToken :: GHC.GenLocated GHC.SrcSpan GHC.Token -> String
showToken t = srcloc ++ " TOKEN= " ++ tok where
  srcloc = show $ GHC.getLoc t
  tok = show $ GHC.unLoc t

