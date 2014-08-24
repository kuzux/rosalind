import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Array (Array, listArray, (!), (//), elems)
import Data.Maybe
import Data.List

bellmanFord :: Int -> [(Int,Int,Int)] -> Array Int (Maybe Int)
bellmanFord n edges = foldl' go initArray [1..n-1]
    where initArray = listArray (1,n) $ Just 0 : (replicate (n-1) Nothing)
          go :: Array Int (Maybe Int) -> Int -> Array Int (Maybe Int)
          go arr _ = foldl' relax arr edges

          relax :: Array Int (Maybe Int) -> (Int,Int,Int) -> Array Int (Maybe Int)
          relax arr (u,v,w) | arr ! u == Nothing = arr
                            | arr ! v == Nothing = arr // [(v, (+) <$> (Just w) <*> arr ! u)]
                            | (fromJust $ arr ! u) + w < (fromJust $ arr ! v) = arr // [(v, (+) <$> (Just w) <*> arr ! u)]
                            | otherwise = arr

main :: IO ()
main = do (n:m:_) <- map read . words <$> getLine
          edges <- replicateM m $ toTuple . map read . words <$> getLine
          putStrLn . unwords . map (maybe "x" show) . elems $ bellmanFord n edges
    where toTuple (a:b:c:_) = (a,b,c)
