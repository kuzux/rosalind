import Control.Monad
import Data.Functor
import Data.List
import Data.Graph

mkGraph :: Int -> [(Int, Int)] -> [(Int, Int, [Int])]
mkGraph n es = [(i, i, go i)| i <- [1..n]]
    where go i = map snd . filter ((==i) . fst) $ es
          fst3 (a,_,_) = a


readGraph :: IO (Int, [(Int, Int)])
readGraph = do _ <- getLine
               (n:m:_) <- map read . words <$> getLine
               edges <- replicateM m $ toTuple . map read . words <$> getLine
               return (n, edges)
    where toTuple (a:b:_) = (a,b)

isAcyclic :: [SCC a] -> Bool
isAcyclic = all acy 
    where acy (AcyclicSCC _) = True
          acy _              = False

main :: IO ()
main = do k <- read <$> getLine
          graphs <- map (\(n,e) -> mkGraph n e) <$> replicateM k readGraph
          putStrLn . unwords . map (disp . isAcyclic . stronglyConnComp) $ graphs
    where disp True  = "1"
          disp False = "-1"

