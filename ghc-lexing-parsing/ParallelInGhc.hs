module Main where

import qualified DynFlags           as GHC
import qualified GHC
import           GHC.Paths          (libdir)
import           MonadUtils
import           GHC.SYB.Utils

import           Control.Parallel.Strategies
import           System.Environment

main :: IO ()
main = do
        [targetFile] <- getArgs
        ghcParseLexParallel targetFile

ghcParseLexParallel :: String -> IO ()
ghcParseLexParallel targetFile =
            GHC.runGhc (Just libdir) $ do
                modSum <- prepareGhc targetFile

                let (x, y) = runEval $ do
                                r_ast <- rpar $ ast modSum
                                r_tokens <- rpar $ tokens modSum
                                _ <- rseq r_ast
                                _ <- rseq r_tokens
                                return (r_ast, r_tokens)
                sx <- x
                sy <- y
                liftIO $ putStrLn $ "finished: " ++ sx ++ sy

                where
                  ast :: GHC.ModSummary -> GHC.Ghc String
                  ast modSum = do
                    p <- GHC.parseModule modSum
                    liftIO $ putStrLn "parsed"
                    return $ showData Parser 2 $ GHC.pm_parsed_source p

                  tokens :: GHC.ModSummary -> GHC.Ghc String
                  tokens modSum = do
                    rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
                    liftIO $ putStrLn "got rts"
                    return $ concatMap showTokenWithSource rts

-- ----------------------------------------------------------------------------------
prepareGhc :: String -> GHC.Ghc GHC.ModSummary
prepareGhc targetFile = do
                dflags <- GHC.getSessionDynFlags
                let dflags' = foldl GHC.xopt_set dflags
                                   [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

                    dflags'' = dflags' { GHC.importPaths = ["./test/testdata/","../test/testdata/"] }

                    dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted,
                                           GHC.ghcLink =  GHC.LinkInMemory }

                _ <- GHC.setSessionDynFlags dflags'''
                liftIO $ putStrLn "dflags set"

                target <- GHC.guessTarget targetFile Nothing
                GHC.setTargets [target]
                liftIO $ putStrLn "targets set"
                _ <- GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
                liftIO $ putStrLn "targets loaded"

                modSum <- GHC.getModSummary $ GHC.mkModuleName "Test"
                liftIO $ putStrLn "got modsummary"
                return modSum

showTokenWithSource :: (GHC.Located GHC.Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ GHC.unLoc loctok
    srcloc = show $ GHC.getLoc loctok
