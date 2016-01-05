
import           Control.Parallel
import           Data.List

pfold :: (Num a, Enum a) => (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold mappend' xs  = (ys `par` zs) `pseq` (ys `mappend'` zs) where
  len = length xs
  (ys', zs') = splitAt (len `div` 2) xs
  ys = pfold mappend' ys'
  zs = pfold mappend' zs'

main :: IO ()
main = print $ pfold (*) [ foldl' (+) 1 [1..x] | x <- [1..50000] :: [Integer]]

-- need a more complicated computation than (+) of numbers
-- so we produce a list of products of many numbers

-- ghc -O2 -rtsopts -threaded ./parFold.hs
-- parFold +RTS -N1 -s
