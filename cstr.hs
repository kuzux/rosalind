import Data.Functor
import Data.List
import Data.Maybe

extractCharacter :: [Char] -> Maybe String
extractCharacter xs | nontrivial chars = Just . buildString $ chars
                    | otherwise        = Nothing
    where chars      = [ (x, count x) | x <- "ACGT", count x /= 0 ]
          count c    = length $ filter (==c) xs
          nontrivial [(_,_)]         = False
          nontrivial ((_,n):(_,m):_) = n /= 1 && m /= 1
          buildString ((a,_):(b,_):_) = map (\x -> if x == a then '1' else '0') xs

characters :: [String] -> [String]
characters = catMaybes . map extractCharacter . transpose

main :: IO ()
main = mapM_ putStrLn =<< characters . lines <$> getContents
