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

main :: IO()
main = do
        [name, line] <- getArgs
        contents <- readUTF8File name
        let ln = read line :: Int
        let ls = lines contents in
          runGhc (Just libdir) $ do
                  flags <- getSessionDynFlags

                  let sb = stringToStringBuffer <$> listElemAt ls ln
                  case sb of
                    Nothing -> GMU.liftIO $ print "NO"
                    Just asb -> do
                      GMU.liftIO $ print $ "line: " ++ line
                      let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1
                      let pResult = lexTokenStream asb lexLoc flags
                      case pResult of

                              POk _ toks    -> GMU.liftIO $ print $ map showToken toks

                              PFailed srcspan msg -> do
                                GMU.liftIO $ print $ show srcspan
                                GMU.liftIO $ do
                                  putStrLn "Lexer Error:"
                                  print $ mkPlainErrMsg flags srcspan msg

showToken :: GenLocated SrcSpan Token -> String
showToken t = srcloc ++ " TOKEN= " ++ tok where
  srcloc = show $ getLoc t
  tok = show $ unLoc t

-- UTF8 utils {{{
readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h

listElemAt :: [a] -> Int -> Maybe a
listElemAt l i = if i >= length l
  then Nothing
  else Just (l !! i)