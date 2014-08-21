import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Functor

addWord :: (Ord a, Integral n) => Map a n -> a -> Map a n
addWord m x = M.insert x (succ val) m
    where val = M.findWithDefault 0 x m 

wordFreqs :: [String] -> Map String Int
wordFreqs = foldl addWord M.empty

main :: IO ()
main = mapM_ putStrLn =<< map disp . M.assocs . wordFreqs . words <$> getContents
    where disp (k,v) = k ++ " " ++ show v

