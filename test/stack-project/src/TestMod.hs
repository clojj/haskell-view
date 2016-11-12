module TestMod
    ( someFunc
    ) where   
 
someFunc :: IO ()
someFunc = do
  let wahr = case (2 == 1 + 1) of
               True -> "true!"
               _    -> "WTF"

  putStrLn "someFunc"

{- A multiline comment
     which can continue for many lines
  -}
func2 :: Int
func2 = undefined

multi = "line1\
\line2\
\line3"   -- a single line comment

