-- FIRST comment
module Test where

f :: Int -> Int
f = \x ->
 x + 1 -- comment 2

doIO :: String -> IO ()
doIO s =
  putStr $ s ++ "äöü multiline...\
    \...string"
-- final comment EOF