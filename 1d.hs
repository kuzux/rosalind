import Data.Functor
import Data.List
import Data.Ord
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

kmers :: Int -> [a] -> [[a]]
kmers k xs = go [] xs
    where go acc xs | length xs < k = acc
                    | otherwise     = go ((take k xs):acc) (tail xs)

kmersWithIndices :: Int -> String -> [(Int,String)]
kmersWithIndices k xs = zip [1..] $ kmers k xs

addWord :: (Ord a) => Map a [b] -> (b, a) -> Map a [b]
addWord m (i, x) = M.insert x (val++[i]) m
    where val = M.findWithDefault [] x m

wordIndices :: [(Int, String)] -> Map String [Int]
wordIndices = foldl' addWord M.empty

fits :: Int -> Int -> Int -> [Int] -> Bool
fits k l t xs = any fits' $ kmers t xs
    where fits' xs = (last xs) - (head xs) + k <= l

main :: IO ()
main = do dna <- getLine
          (k:l:t:_) <- map read . words <$> getLine
          putStrLn . unwords . M.keys . M.filter (fits k l t) . wordIndices . kmersWithIndices k $ dna

