{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TokenGenerator where

import qualified GHC
import qualified MonadUtils            as GHC
import qualified Lexer                 as GHC

import Data.Data

-- Data, Typeable for GHC.Token
deriving instance Data GHC.Token
deriving instance Typeable GHC.Token


main = 
  print $ show $ dataTypeConstrs $ dataTypeOf GHC.ITsemi
  
  -- TODO generate for Elm: tokens.css (and maybe some mapping case ... of) 
