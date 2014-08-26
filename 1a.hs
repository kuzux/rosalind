import Data.Functor
import Data.List
import Data.Ord
import Data.Map.Strict (Map)
import qualified Data.Map as M

kmers :: Int -> String -> [String]
kmers k xs = go [] xs
    where go acc xs | length xs < k = acc
                    | otherwise     = go ((take k xs):acc) (tail xs)

addWord :: (Ord a, Integral n) => Map a n -> a -> Map a n
addWord m x = M.insert x (succ val) m
    where val = M.findWithDefault 0 x m 

wordFreqs :: [String] -> Map String Int
wordFreqs = foldl addWord M.empty

mostFrequent :: Map String Int -> [String]
mostFrequent m = map fst . filter ((==max) . snd) . M.assocs $ m
    where max = maximum . M.elems $ m

main :: IO ()
main = do dna <- getLine
          k <- read <$> getLine
          putStrLn . unwords . mostFrequent . wordFreqs . kmers k $ dna

