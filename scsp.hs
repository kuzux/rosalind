import Data.Array (Array, (!), array, range)
import Data.Functor
import Prelude hiding (Either(..))

data Direction = Up | Left | Diagonal | Stop deriving (Eq, Show)

scsTable :: Eq a => [a] -> [a] -> Array (Int,Int) (Int, Direction)
scsTable xs ys = table
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
    dist (0,0) = (0, Stop)
    dist (0,j) = (j, Left)
    dist (i,0) = (i, Up)
    dist (i,j)
        | minVal == d  = (1 + d, Diagonal)
        | minVal == u  = (1 + u, Left)
        | otherwise    = (1 + l, Up)
        where d = if x ! i == y ! j then tableVal (i-1, j-1) else m+n
              u = tableVal (i, j-1)
              l = tableVal (i-1, j)
              minVal = minimum [d,u,l]
    
findString :: [a] -> [a] -> Array (Int,Int) (Int,Direction) -> [a]
findString xs ys table = go (m,n)
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)

    --go :: (Int,Int) -> [a]
    go ij@(i,j) = case snd (table ! ij) of 
        Stop     -> []
        Up       -> go (i-1, j) ++ [x!i]
        Left     -> go (i, j-1) ++ [y!j]
        Diagonal -> go (i-1, j-1) ++ [x!i]

main :: IO ()
main = do (xs:ys:_) <- lines <$> getContents
          let tab = scsTable xs ys
          --print tab
          --print . fst $ tab ! (length xs, length ys)
          putStrLn . findString xs ys $ tab

