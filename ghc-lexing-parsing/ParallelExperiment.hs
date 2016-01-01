module Main where

import qualified DynFlags           as GHC
import qualified GHC
import           GHC.Paths          (libdir)
import           MonadUtils
import           GHC.SYB.Utils

import           Control.Concurrent
import           Control.Parallel.Strategies
import           System.Environment

main :: IO ()
main = do
        [targetFile] <- getArgs
        let (x, y) = runEval $ run targetFile
        sx <- x
        sy <- y
        putStrLn $ "finished: " ++ sx ++ sy

run :: String -> Eval (IO String, IO String)
run targetFile = do
  x <- rpar $ ghcParseString targetFile
  y <- rpar $ ghcLexString targetFile
  _ <- rseq x
  _ <- rseq y
  return (x,y)

ghcParseString :: String -> IO String
ghcParseString targetFile =
            GHC.runGhc (Just libdir) $ do
                modSum <- prepareGhc targetFile

                p <- GHC.parseModule modSum
                liftIO $ putStrLn "parsed"
                return $ showData Parser 2 $ GHC.pm_parsed_source p

ghcLexString :: String -> IO String
ghcLexString targetFile =
            GHC.runGhc (Just libdir) $ do
                modSum <- prepareGhc targetFile

                rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
                liftIO $ putStrLn "got rts"
                return $ concatMap showTokenWithSource rts


-- ----------------------------------------------------------------------------------
runConcurrent :: String -> IO ()
runConcurrent targetFile = do
        _ <- forkIO $ ghcLex targetFile
        _ <- forkIO $ ghcParse targetFile
        print "finished"

ghcParse :: String -> IO ()
ghcParse targetFile =
        GHC.runGhc (Just libdir) $ do
                modSum <- prepareGhc targetFile

                p <- GHC.parseModule modSum
                liftIO $ putStrLn "parsed"
                liftIO $ putStrLn $ showData Parser 2 $ GHC.pm_parsed_source p

ghcLex :: String -> IO ()
ghcLex targetFile =
            GHC.runGhc (Just libdir) $ do
                modSum <- prepareGhc targetFile

                rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
                liftIO $ putStrLn "got rts"
                liftIO $ putStr $ concatMap showTokenWithSource rts

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
