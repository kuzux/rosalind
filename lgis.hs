{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array (Array, (!), array, assocs)
import Data.Functor
import Data.List
import Data.Ord

lgis :: forall a. (Ord a) => [a] -> [a]
lgis xs = backtrack . fst . maximumBy (comparing $ snd . snd) . assocs $ lis
    where
    n = length xs
    
    x = array (1,n) $ zip [1..] xs

    -- first element of tuple is backtrack, second is length
    lis :: Array Int (Int, Int)
    lis = array (1,n) [ (i, go i) | i <- [1..n] ]

    go :: Int -> (Int, Int)
    go i = case filter fits [1..(i-1)] of
                [] -> (-1, 1)
                ts -> maximumBy (comparing snd) $ map calc ts
           where fits j = x ! j < x ! i
                 calc j = (j, snd (lis ! j) + 1)

    backtrack :: Int -> [a]
    backtrack i | (fst $ lis ! i) == -1 = [x ! i]
                | otherwise           = backtrack (fst $ lis ! i) ++ [ x ! i ]

main :: IO ()
main = do _  <- getLine
          xs <- map read . words <$> getLine :: IO [Integer]
          putStrLn . unwords . map show . lgis $ xs
          putStrLn . unwords . map show . reverse . lgis . reverse $ xs
