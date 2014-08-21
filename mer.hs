import Data.Functor

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) 
    | x < y     = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

main :: IO ()
main = do getLine
          xs <- (map read . words <$> getLine) :: IO [Integer]
          getLine
          ys <- (map read . words <$> getLine) :: IO [Integer]
          putStrLn . unwords . map show $ merge xs ys
