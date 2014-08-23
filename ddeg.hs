import Control.Monad
import Data.Array (Array, array, (!))
import Data.Functor

adjacentEdges :: Int -> [(Int,Int)] -> [(Int,Int)]
adjacentEdges x = filter (\(a,b) -> a == x || b == x)

degrees :: Int -> [(Int,Int)] -> Array Int Int
degrees n xs = array (1,n) [(i, deg i) | i <- [1..n]]
    where deg i = length $ adjacentEdges i xs

neighbors :: [(Int, Int)] -> Int -> [Int]
neighbors es x = map other . adjacentEdges x $ es
    where other (a,b) = if a == x then b else a

main :: IO()
main = do (n:m:_) <- map read . words <$> getLine
          edges <- map (\(x:y:_) -> (x,y)) <$> replicateM m (map read . words <$> getLine) :: IO [(Int, Int)]
          let degs = degrees n edges
          putStrLn . unwords . map (show . sum . map (degs!) . neighbors edges) $ [1..n]

