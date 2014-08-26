import Control.Monad
import Data.Functor
import Data.Graph

mkGraph :: Int -> [(Int, Int)] -> Graph
mkGraph n es = fst3 . graphFromEdges $ [(i, i, go i)| i <- [1..n]]
    where go i = map snd . filter ((==i) . fst) $ es
          fst3 (a,_,_) = a


readGraph :: IO (Int, [(Int, Int)])
readGraph = do (n:m:_) <- map read . words <$> getLine
               edges <- replicateM m $ toTuple . map read . words <$> getLine
               return (n, edges)
    where toTuple (a:b:_) = (a,b)

main :: IO ()
main = putStrLn =<< unwords . map (show . succ) . topSort . (\(n,e) -> mkGraph n e) <$> readGraph
          
          
