import Data.Functor
import Data.List

skews :: String -> [Int]
skews = scanl go 0
    where go acc x | x == 'C'  = acc-1
                   | x == 'G'  = acc+1
                   | otherwise = acc

minSkews :: [Int] -> [Int]
minSkews xs = map fst . filter ((==min) . snd). zip [0..] $ xs
    where min = minimum xs

main :: IO ()
main = putStrLn =<< unwords . map show . minSkews . skews <$> getLine
