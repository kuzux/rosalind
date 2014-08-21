import Data.List
import Data.Functor

nums :: Char -> Integer
nums 'A' = 4
nums 'C' = 2
nums 'D' = 2
nums 'E' = 2
nums 'F' = 2
nums 'G' = 4
nums 'H' = 2
nums 'I' = 3
nums 'K' = 2
nums 'L' = 6
nums 'M' = 1
nums 'N' = 2
nums 'P' = 4
nums 'Q' = 2
nums 'R' = 6
nums 'S' = 6
nums 'T' = 4
nums 'V' = 4
nums 'W' = 1
nums 'Y' = 2

numRNAs :: String -> Integer
numRNAs = foldl (\a x -> (a * nums x) `mod` 1000000) 3

main :: IO ()
main = print =<< numRNAs <$> getLine
