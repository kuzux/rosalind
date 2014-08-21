import Data.Array (Array, (!), array, range)
import Data.Functor
import Fasta
import Prelude hiding (Either(..))

data Direction = Up | Left | Diagonal | Stop deriving (Eq, Show)

lcsTable :: Eq a => [a] -> [a] -> Array (Int,Int) (Int, Direction)
lcsTable xs ys = table
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
 
    table :: Array (Int,Int) (Int, Direction)
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))

    tableVal :: (Int,Int) -> Int
    tableVal ij = fst $ table ! ij
  
    dist :: (Int, Int) -> (Int, Direction)
    dist (0,j) = (0, Stop)
    dist (i,0) = (0, Stop)
    dist (i,j)
        | x ! i == y ! j                        = (1 + tableVal (i-1, j-1), Diagonal)
        | tableVal (i, j-1) < tableVal (i-1, j) = (tableVal (i-1, j), Up)
        | otherwise                             = (tableVal (i, j-1), Left)
    
findString :: [a] -> [a] -> Array (Int,Int) (Int,Direction) -> [a]
findString xs ys table = go (m,n)
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)

    --go :: (Int,Int) -> [a]
    go ij@(i,j) = case snd (table ! ij) of 
        Stop     -> []
        Up       -> go (i-1, j) 
        Left     -> go (i, j-1)
        Diagonal -> go (i-1, j-1) ++ [x!i]

main :: IO ()
main = do (xs:ys:_) <- parseAsStrings <$> getContents
          putStrLn . findString xs ys $ lcsTable xs ys

