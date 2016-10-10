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

import Lib

type IOAPI =
  "source" :> Capture "modulename" String :> Get '[PlainText] String :<|> -- TODO parameterize
  "view" :> Raw

ioAPI :: Proxy IOAPI
ioAPI = Proxy

server :: Server IOAPI
server = getFile :<|> serveDirectory "webclient"

type MyHandler = ExceptT ServantErr IO

-- TODO parameterize
getFile :: String -> MyHandler String
getFile modulename = do
  liftIO (readFile $ "webclient/docroot/" ++ modulename)

app :: Application
app = serve ioAPI server

main :: IO ()
main = do
  ghcMain
  args <- getArgs
  case args of
    _ -> run 8081 app

