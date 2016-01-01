module Main where

import           System.Environment (getArgs)

import qualified DynFlags           as GHC
import           GHC
import           GHC.Paths          (libdir)
import qualified MonadUtils         as GMU

-- ITvocurly ("virtual" braces for layout induced blocks)
-- ITocurly (real braces for no-layout blocks)
-- same for ITvccurly

main :: IO()
main = do
        [targetFile] <- getArgs

        runGhc (Just libdir) $ do
                dflags <- GHC.getSessionDynFlags
                let dflags' = foldl GHC.xopt_set dflags
                                   [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

                    dflags'' = dflags' { GHC.importPaths = ["./test/testdata/","../test/testdata/"] }

                    dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted,
                                           GHC.ghcLink =  GHC.LinkInMemory }

                GHC.setSessionDynFlags dflags'''
                GMU.liftIO $ putStrLn $ "dflags set"

                target <- GHC.guessTarget targetFile Nothing
                GHC.setTargets [target]
                GMU.liftIO $ putStrLn $ "targets set"
                GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
                GMU.liftIO $ putStrLn $ "targets loaded"

                modSum <- GHC.getModSummary $ GHC.mkModuleName "Test"
                GMU.liftIO $ putStrLn $ "got modsummary"
                p <- GHC.parseModule modSum
                GMU.liftIO $ putStrLn $ "parsed"

                -- Tokens ------------------------------------------------------
                ts <- GHC.getTokenStream (GHC.ms_mod modSum)
                GMU.liftIO $ putStrLn $ "got ts"
                rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
                GMU.liftIO $ putStrLn $ "got rts"

                GMU.liftIO $ putStr $ concatMap showTokenWithSource rts


showTokenWithSource :: (Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok
