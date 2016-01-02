module Main where

import           System.Environment (getArgs)

import qualified DynFlags           as GHC
import qualified GHC
import           GHC.Paths          (libdir)
import           GHC.SYB.Utils
import           MonadUtils

-- ITvocurly ("virtual" braces for layout induced blocks)
-- ITocurly (real braces for no-layout blocks)
-- same for ITvccurly

main :: IO()
main = do
        [targetFile] <- getArgs

        GHC.runGhc (Just libdir) $ do
                dflags <- GHC.getSessionDynFlags
                let dflags' = foldl GHC.xopt_set dflags
                                   [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

                    -- dflags'' = dflags' { GHC.importPaths = ["./test/testdata/","../test/testdata/"] }
                    --
                    -- dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted,
                    --                        GHC.ghcLink =  GHC.LinkInMemory }

                _ <- GHC.setSessionDynFlags dflags'

                target <- GHC.guessTarget targetFile Nothing
                GHC.setTargets [target]
                _ <- GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
                liftIO $ putStrLn "targets loaded"

                modSum <- GHC.getModSummary $ GHC.mkModuleName "Test"
                liftIO $ putStrLn "got modsummary"

                -- AST -- ------------------------------------------------------
                p <- GHC.parseModule modSum
                liftIO $ putStrLn $ showData Parser 2 $ GHC.pm_parsed_source p

                -- Tokens ------------------------------------------------------
                rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
                liftIO $ putStr $ concatMap showTokenWithSource rts


showTokenWithSource :: (GHC.Located GHC.Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ GHC.unLoc loctok
    srcloc = show $ GHC.getLoc loctok
