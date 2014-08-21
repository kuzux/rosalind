import Data.Functor
import Data.Maybe 
import Fasta

find' :: (Eq a) => [(Int, a)] -> [a] -> Maybe [Int]
find' _ [] = Just []
find' [] _ = Nothing
find' ((i,x):xs) (y:ys)
    | x == y    = (i:) <$> find' xs ys
    | otherwise = find' xs (y:ys)

find :: (Eq a) => [a] -> [a] -> [Int]
find x y = fromJust $ find' (zip [1..] x) y

main :: IO ()
main = do (x:y:_) <- parseAsStrings <$> getContents
          putStrLn . unwords . map show $ find x y
