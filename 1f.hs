import Data.Functor
import Data.List

hammingDistance :: String -> String -> Int
hammingDistance x y = sum $ zipWith (\a b->if a==b then 0 else 1) x y

approximateMatches :: Int -> String -> String -> [Int]
approximateMatches k needle haystack = go (0,[]) haystack
    where len = length needle
          go (n, acc) xs | length xs < len                           = acc
                         | hammingDistance needle (take len xs) <= k = go (n+1, (n:acc)) (tail xs)
                         | otherwise                                 = go (n+1, acc)     (tail xs)
           
main :: IO ()
main = do needle <- getLine
          haystack <- getLine
          k <- read <$> getLine
          putStrLn . unwords . map show . reverse $ approximateMatches k needle haystack

