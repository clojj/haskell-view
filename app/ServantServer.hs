{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import Servant
import System.Directory

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

type IOAPI =
  "testMod" :> Get '[PlainText] String :<|> -- TODO parameterize
  "view" :> Raw

ioAPI :: Proxy IOAPI
ioAPI = Proxy

server :: Server IOAPI
server = getFile :<|> serveDirectory "webclient"

type MyHandler = ExceptT ServantErr IO

-- TODO parameterize
getFile :: MyHandler String
getFile = do
  liftIO (readFile "webclient/docroot/TestMod")

app :: Application
app = serve ioAPI server

main :: IO ()
main = do
  ghcMain
  args <- getArgs
  case args of
    _ -> run 8081 app

ghcMain :: IO ()
ghcMain =
    -- TODO send errors/exceptions/messages to client !
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $

      GHC.liftIO $ GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = dflags -- foldl GHC.xopt_set dflags [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]
            dflags'' = dflags' { GHC.importPaths = ["./test/testdata/"] }
            dflags''' = dflags'' { GHC.hscTarget = GHC.HscNothing }
            -- dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink =  GHC.LinkInMemory }

        GHC.setSessionDynFlags dflags'''

        target <- GHC.guessTarget targetFile Nothing
        GHC.setTargets [target]
        let modName = GHC.mkModuleName moduleName
        GHC.load $ GHC.LoadUpTo modName
        -- GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO (putStrLn "targets loaded")

        modSum <- GHC.getModSummary modName

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
        GHC.liftIO (putStrLn lexedSource)


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

