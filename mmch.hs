import Data.List
import Data.Functor
import Fasta

numPerms :: Integer -> Integer -> Integer
numPerms n k = product [n-k+1..n] 

numMatchings :: String -> Integer
numMatchings xs = pa * pg
    where na = genericLength $ filter (=='A') xs
          nc = genericLength $ filter (=='C') xs 
          ng = genericLength $ filter (=='G') xs 
          nu = genericLength $ filter (=='U') xs 
          pa | na > nu   = numPerms na nu
             | otherwise = numPerms nu na
          pg | ng > nc   = numPerms ng nc
             | otherwise = numPerms nc ng

main :: IO ()
main = print =<< numMatchings . head . parseAsStrings <$> getContents

