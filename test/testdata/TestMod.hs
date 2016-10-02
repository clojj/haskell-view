module TestMod
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- A multiline comment
     which can continue for many lines
  -}
multi = "line1\
\line2\
\line3"   -- a single line comment
