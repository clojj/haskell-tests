module Main where

import           System.Environment (getArgs)
import           System.IO

import           ErrUtils           (mkPlainErrMsg)
import           FastString         (mkFastString)
import           GHC
import           GHC.Paths          (libdir)
import           Lexer
import qualified MonadUtils         as GMU
import           SrcLoc
import           StringBuffer

-- ITvocurly ("virtual" braces for layout induced blocks)
-- ITocurly (real braces for no-layout blocks)
-- same for ITvccurly

main :: IO()
main = do
        [name, offset] <- getArgs
        contents <- readUTF8File name

        runGhc (Just libdir) $ do
                flags <- getSessionDynFlags
                let sb = stringToStringBuffer contents
                let off = read offset :: Int
                GMU.liftIO $ print $ "offset: " ++ offset
                let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1

                let pResult = lexTokenStream (offsetBytes off sb) lexLoc flags
                case pResult of

                        -- POk _ toks    -> GMU.liftIO $ print $ map showToken toks
                        POk _ toks -> GMU.liftIO $ putStr $ concatMap showTokenWithSource (addSourceToTokens lexLoc sb toks)

                        PFailed srcspan msg -> do
                          GMU.liftIO $ print $ show srcspan
                          GMU.liftIO $ do
                            putStrLn "Lexer Error:"
                            print $ mkPlainErrMsg flags srcspan msg

showToken :: GenLocated SrcSpan Token -> String
showToken t = srcloc ++ " TOKEN= " ++ tok where
  srcloc = show $ getLoc t
  tok = show $ unLoc t

showTokenWithSource :: (Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok

-- UTF8 utils {{{
readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h
