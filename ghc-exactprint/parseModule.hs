{-# LANGUAGE RankNTypes #-}
module Main where

import           System.Environment

import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Utils

import qualified GHC

main :: IO ()
main = do
  (arg1 : _) <- getArgs
  runPipe arg1

runPipe :: FilePath -> IO ()
runPipe file = do
    Right (as, m) <- parseModule file
    putStrLn (showAnnData as 0 m)
    -- putStrLn (showGhc as)

runPipe' :: FilePath -> IO (Either (GHC.SrcSpan, String) (Anns, GHC.ParsedSource))
runPipe' file = do
    p <- parseModule file
    return (either Left Right p)
