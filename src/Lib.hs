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

        p <- GHC.parseModule modSum
        let ps  = GHC.pm_parsed_source p
        GHC.liftIO (putStrLn $ "ParsedSource\n\n" ++ SYB.showData SYB.Parser 0 ps)

        -- Tokens ------------------------------------------------------

        -- TODO check if fixed ? http://ghc.haskell.org/trac/ghc/ticket/8265
        -- rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        rts <- HaRe.getRichTokenStreamWA (GHC.ms_mod modSum)

        -- let tokens_and_source =
        --      GHC.addSourceToTokens (GHC.mkRealSrcLoc (GHC.mkFastString "<file>") 1 1) (GHC.stringToStringBuffer "") ts
        -- GHC.liftIO $ putStrLn $ "addSourceToTokens=" ++ concatMap showRichToken tokens_and_source
        -- GHC.liftIO (putStrLn $ concatMap showRichToken rts)

        -- TODO sourceId: filename, path, module-name, ...?
        let sourceId = moduleName
        let lexedSource = sourceId ++ "\n" ++ GHC.showRichTokenStream rts ++ "<EOF>\n" ++ concatMap (("\n" ++).(++ "\n").showToken) rts
        -- GHC.liftIO (putStrLn lexedSource)
        GHC.liftIO $ writeFile ("./webclient/docroot/" ++ moduleName) lexedSource


showToken :: (GHC.Located GHC.Token, String) -> String
showToken (t, s) = tok ++ "\n" ++ srcloc where
  srcloc = show $ GHC.getLoc t
  tok = show $ GHC.unLoc t


-- useful helper functions (?)

tokenLocs = map (\(GHC.L l _, s) -> (l,s))

showRichToken :: (GHC.Located GHC.Token, String) -> String
showRichToken (loc_tok, s) =
  "\n\nTOKEN " ++ tok
  ++ "\nSRC " ++ "'" ++ s ++ "'"
  ++ "\nLOC " ++ srcloc
    where
      srcloc = show $ GHC.getLoc loc_tok
      tok = show $ GHC.unLoc loc_tok

