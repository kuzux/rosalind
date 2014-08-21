import Data.Array (Array, array, range, (!))
import Data.Functor
import Fasta

matches :: Char -> Char -> Bool
matches 'A' 'U' = True
matches 'U' 'A' = True
matches 'C' 'G' = True
matches 'G' 'C' = True
matches  _   _  = False

numMatches :: String -> Integer
numMatches xs = table ! (0,n-1)
    where 
    n = length xs

    x = array (0,n-1) (zip [0..] xs)

    table :: Array (Int, Int) Integer
    table = array bnds [ (ij, go ij) | ij <- range bnds ]
    bnds = ((0,0), (n,n))

    coeff :: (Int,Int) -> Integer
    coeff (i,j) | matches (x ! i) (x ! j) = 1
                | otherwise               = 0

    go :: (Int, Int) -> Integer
    go ij@(i,j) | j <= i           = 1
                | i >= n || j >= n = 0
                | otherwise        = (table ! (i+1, j)) + sum [ (coeff (i,k)) * (table ! (i+1,k-1)) * (table ! (k+1,j)) | k <- [(i+1)..j] ]

main :: IO ()
main = print =<< (`mod` 1000000) . numMatches . head . parseAsStrings <$> getContents
