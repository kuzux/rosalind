import Data.Array (Array, (!), array, range, bounds)
import Data.List
import Data.Functor
import Fasta

data Change = Add | Remove | Modify | Stop deriving (Eq, Show)

scsTable :: Eq a => [a] -> [a] -> Array (Int,Int) (Int, Change)
scsTable xs ys = table
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
 
    table :: Array (Int,Int) (Int, Change)
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))

    tableVal :: (Int,Int) -> Int
    tableVal ij = fst $ table ! ij
  
    dist :: (Int, Int) -> (Int, Change)
    dist (0,0) = (0, Stop)
    dist (0,j) = (j, Add)
    dist (i,0) = (i, Remove)
    dist (i,j)
        | x ! i == y ! j = (tableVal (i-1,j-1), Modify)
        | minVal == l    = (1+l, Add)
        | minVal == u    = (1+u, Remove)
        | otherwise      = (1+d, Modify)
        where l = tableVal (i, j-1)
              u = tableVal (i-1, j)
              d = if x ! i == y ! j then m+n else tableVal (i-1,j-1)
              minVal = minimum [u,l,d]

modifications :: Array (Int,Int) (Int, Change) -> [Change]
modifications table = go end
    where end = snd . bounds $ table
          go ij@(i,j) = case snd (table ! ij) of
              Stop   -> []
              Add    -> go (i,j-1) ++ [Add]
              Remove -> go (i-1,j) ++ [Remove]
              Modify -> go (i-1,j-1) ++ [Modify]

buildStrings :: String -> String -> [Change] -> (String, String)
buildStrings x y cs = fst3 $ foldl' go (("",""), x, y) cs
    where go ((xAcc, yAcc), xs, ys) c = case c of
              Modify -> ((xAcc ++ [head xs], yAcc ++ [head ys]), tail xs, tail ys)
              Add    -> ((xAcc ++ "-", yAcc ++ [head ys]), xs, tail ys)
              Remove -> ((xAcc ++ [head xs], yAcc ++ "-"), tail xs, ys)
          fst3 (a,_,_) = a

main :: IO ()
main = do (xs:ys:_) <- parseAsStrings <$> getContents
          let tab = scsTable xs ys
          print . fst $ tab ! (length xs, length ys)
          let (x', y') = buildStrings xs ys . modifications $ tab 
          putStrLn x'
          putStrLn y'

