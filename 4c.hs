import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

kmers :: Int -> String -> [String]
kmers k xs = go [] xs
    where go acc xs | length xs < k = acc
                    | otherwise     = go ((take k xs):acc) (tail xs)

addWord :: Map String [String] -> String -> Map String [String]
addWord m k = M.insert x (y:val) m
    where val   = M.findWithDefault [] x m 
          (x,y) = (init k, tail k)

constructGraph :: [String] -> Map String [String]
constructGraph = foldl' addWord M.empty

main :: IO ()
main = do k <- read <$> getLine
          dna <- getLine
          mapM_ (putStrLn . disp) . M.assocs . constructGraph . nub . kmers k $ dna
    where disp (n, es) = n ++ " -> " ++ (intercalate "," es)

