-- FIRST comment
module Test where

import TestBad

f :: Int -> Int
f = \x ->
 x + 1 -- comment 2

doIO :: String -> IO ()
doIO s =
  putStr $ s ++ "äöü\
  \222 3"
-- final comment EOF