module Main where

import           System.Environment (getArgs)

import qualified DynFlags           as GHC
import qualified GHC
import           GHC.Paths          (libdir)
import           GHC.SYB.Utils
import           MonadUtils
import           Outputable

-- ITvocurly ("virtual" braces for layout induced blocks)
-- ITocurly (real braces for no-layout blocks)
-- same for ITvccurly

main :: IO()
main = do
        [targetFile, tf2] <- getArgs

        GHC.runGhc (Just libdir) $ do
                dflags <- GHC.getSessionDynFlags
                let dflags' = foldl GHC.xopt_set dflags
                                   [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

                    -- dflags'' = dflags' { GHC.importPaths = ["./test/testdata/","../test/testdata/"] }
                    --
                    dflags'' = dflags' { GHC.hscTarget = GHC.HscInterpreted,
                                           GHC.ghcLink =  GHC.LinkInMemory }

                _ <- GHC.setSessionDynFlags dflags''

                target <- GHC.guessTarget targetFile Nothing
                target2 <- GHC.guessTarget tf2 Nothing
                GHC.setTargets [target, target2]
                _ <- GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
                liftIO $ putStrLn "targets loaded"

                modSum <- GHC.getModSummary $ GHC.mkModuleName "Test"
                liftIO $ putStrLn $ "ModSummary: " ++ showSDoc dflags'' (ppr modSum)
                liftIO $ putStrLn $ "ms_hsc_src " ++ show (GHC.ms_hsc_src modSum)
                -- liftIO $ putStrLn $ "ms_srcimps " ++ (concatMap (showLocated dflags')  (GHC.ms_srcimps modSum))
                liftIO $ putStrLn $ "ms_hspp_file " ++ show (GHC.ms_hspp_file modSum)

                -- AST -- ------------------------------------------------------
                p <- GHC.parseModule modSum
                liftIO $ putStrLn $ showData Parser 2 $ GHC.pm_parsed_source p

                -- Tokens ------------------------------------------------------
                rts <- GHC.getRichTokenStream (GHC.ms_mod modSum)
                liftIO $ putStr $ concatMap showTokenWithSource rts

                -- modify .hs
                _ <- liftIO getLine

                -- target <- GHC.guessTarget targetFile Nothing
                -- GHC.setTargets [target]
                -- _ <- GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
                -- liftIO $ putStrLn "target loaded"

                -- modSum <- GHC.getModSummary $ GHC.mkModuleName "Test"
                -- liftIO $ putStrLn "got modsummary"

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
  "\n\n"
   where
    tok = show $ GHC.unLoc loctok
    srcloc = show $ GHC.getLoc loctok

showLocated :: GHC.DynFlags -> GHC.Located a -> String
showLocated flags (GHC.L l e) = showSDoc flags (ppr l)
