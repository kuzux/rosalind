import Control.Monad
import Data.Functor
import Data.Graph

mkGraph :: Int -> [(Int, Int)] -> Graph
mkGraph n es = fst3 . graphFromEdges $ [(i, i, go i)| i <- [1..n]]
    where go i = map snd . filter ((==i) . fst) $ es
          fst3 (a,_,_) = a

main :: IO ()
main = do (n:m:_) <- map read . words <$> getLine
          graph <- mkGraph n . map (\(x:y:_) -> (x,y)) <$> replicateM m (map read . words <$> getLine)
          print . length . components $ graph

