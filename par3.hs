import Data.List
import Data.Functor

par3 :: (Ord a) => [a] -> [a]
par3 (x:xs) = lo ++ [x] ++ mid ++ hi
    where (lo',hi) = partition (<=x) xs
          (lo, mid) = partition (<x) lo'

main :: IO ()
main = do getLine
          xs <- map read . words <$> getLine :: IO [Integer]
          putStrLn . unwords . map show . par3 $ xs
