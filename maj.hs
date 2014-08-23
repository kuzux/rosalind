import Control.Monad
import Data.List
import Data.Functor
import Data.Maybe

median :: (Ord a) => [a] -> a
median xs = (sort xs) !! m
    where m = (length xs) `div` 2

majority :: (Ord a) => [a] -> Maybe a
majority xs = maj' . median $ xs
    where maj' c | (length . filter (==c) $ xs) > (n `div` 2) = Just c
                 | otherwise                                  = Nothing
          n      = length xs

main :: IO ()
main = do (k:_) <- map read . words <$> getLine
          xss <- replicateM k (map read . words <$> getLine) :: IO [[Int]]
          putStrLn . unwords . map (maybe "-1" show . majority) $ xss 
