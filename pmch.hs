import Fasta
import Data.Functor

numMatchings :: String -> Integer
numMatchings xs = (fact a)*(fact g)
    where a = length $ filter (=='A') xs
          g = length $ filter (=='G') xs
          fact n = product [1..(fromIntegral n)]

main :: IO ()
main = print =<< numMatchings . head . parseAsStrings <$> getContents
