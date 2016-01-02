-- FIRST comment
module Test where

import TestBad

f :: Int -> Int
f = \x ->
 x + 1 -- comment 2

doIO :: String -> IO ()
doIO str =
  putStr $ str ++ "äöü multiline\
    \222"
-- final comment EOF