import Data.List
import Data.Functor

par :: (Ord a) => [a] -> [a]
par (x:xs) = lo ++ [x] ++ hi
    where (lo,hi) = partition (<=x) xs

main :: IO ()
main = do getLine
          xs <- map read . words <$> getLine :: IO [Integer]
          putStrLn . unwords . map show . par $ xs
