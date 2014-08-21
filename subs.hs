import Data.List
import Control.Applicative

substrings :: (Eq a) => [a] -> [a] -> [Int]
substrings haystack needle = map fst . filter matches . zip [1..] . map (take len) $ tails haystack
    where len = length needle
          matches (_,x) = needle == x

main :: IO ()
main = putStrLn =<< (intercalate " " . map show) <$> (substrings <$> getLine <*> getLine)

