import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

addWord :: Map String [String] -> String -> Map String [String]
addWord m k = M.insert x (y:val) m
    where val   = M.findWithDefault [] x m 
          (x,y) = (init k, tail k)

constructGraph :: [String] -> Map String [String]
constructGraph = foldl' addWord M.empty

main :: IO ()
main = do kmers <- lines <$> getContents
          mapM_ (putStrLn . disp) . M.assocs . constructGraph . nub $ kmers
    where disp (n, es) = n ++ " -> " ++ (intercalate "," es)

