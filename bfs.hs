{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Data.Array (Array, array, (!))
import Data.Functor
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Sequence (Seq, viewl, ViewL(..), (><))
import qualified Data.Sequence as Seq

type Graph = Array Int [Int] -- adjacency list format

mkGraph :: Int -> [(Int,Int)] -> Graph
mkGraph n es = array (1,n) [(i, go i) | i <- [1..n]]
    where go i = map snd . filter ((==i) . fst) $ es

shortestPaths :: Graph -> IntMap Int
shortestPaths graph = go (IM.empty) (Seq.singleton (1,0))
    where go ds (viewl -> EmptyL)      = ds
          go ds (viewl -> (x,d) :< xs) | x `IM.member` ds = go ds xs
                                       | otherwise        = go ds' xs'
             where ds' = IM.insert x d ds
                   xs' = xs >< Seq.fromList [ (y, d+1) | y <- graph ! x, not $ y `IM.member` ds ]

main :: IO ()
main = do (n:m:_) <- map read . words <$> getLine
          graph <- mkGraph n . map (\(x:y:_) -> (x,y)) <$> replicateM m (map read . words <$> getLine)
          let paths = shortestPaths graph
          putStrLn . unwords $ [ maybe "-1" show $ IM.lookup x paths | x <- [1..n]]

