{-# LANGUAGE ViewPatterns #-}

-- This doesn't work, I really don't have no idea why. 

import Control.Monad
import Data.Array (Array, array, (!))
import Data.Functor
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Maybe
import Data.PSQueue (Binding(..))
import qualified Data.PSQueue as PQ

type Graph = Array Int [(Int,Int)] -- adjacency list format, fst = node, snd = length

mkGraph :: Int -> [(Int,Int,Int)] -> Graph
mkGraph n es = array (1,n) [(i, go i) | i <- [1..n]]
    where go i = map edge . filter ((==i) . fst3) $ es
          fst3 (a,_,_) = a
          edge (_,b,c) = (b,c)

shortestPaths :: Graph -> IntMap Int
shortestPaths graph = go (IM.empty) (PQ.singleton 1 0)
    where go ds (PQ.minView -> Nothing)            = ds
          go ds (PQ.minView -> Just (x :-> d, xs)) | x `IM.member` ds = go ds xs
                                                   | otherwise        = go ds' xs'
             where ds' = IM.insert x d ds
                   xs' = foldl' ins' xs [(y, d+d') | (y,d') <- graph ! x, not $ y `IM.member` ds]
                   ins' acc (x',d') = PQ.insert x' d' acc

main :: IO ()
main = do (n:m:_) <- map read . words <$> getLine
          graph <- mkGraph n . map (\(x:y:d:_) -> (x,y,d)) <$> replicateM m (map read . words <$> getLine)
          let paths = shortestPaths graph
          putStrLn . unwords $ [ maybe "-1" show $ IM.lookup x paths | x <- [1..n]]

