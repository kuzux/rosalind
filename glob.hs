import Control.Applicative
import Data.Array (Array, (!), array, range, bounds)
import Data.List
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Fasta

data Change = Add | Remove | Modify | Stop deriving (Eq, Show)

blosum62 :: Map (Char, Char) Int
blosum62 = Map.fromList . zip ((,) <$> prots <*> prots) . concat $ table
    where prots = ['A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y']
          table = [[4,0,-2,-1,-2,0,-2,-1,-1,-1,-1,-2,-1,-1,-1,1,0,0,-3,-2]
                  ,[0,9,-3,-4,-2,-3,-3,-1,-3,-1,-1,-3,-3,-3,-3,-1,-1,-1,-2,-2]
                  ,[-2,-3,6,2,-3,-1,-1,-3,-1,-4,-3,1,-1,0,-2,0,-1,-3,-4,-3]
                  ,[-1,-4,2,5,-3,-2,0,-3,1,-3,-2,0,-1,2,0,0,-1,-2,-3,-2]
                  ,[-2,-2,-3,-3,6,-3,-1,0,-3,0,0,-3,-4,-3,-3,-2,-2,-1,1,3]
                  ,[0,-3,-1,-2,-3,6,-2,-4,-2,-4,-3,0,-2,-2,-2,0,-2,-3,-2,-3]
                  ,[-2,-3,-1,0,-1,-2,8,-3,-1,-3,-2,1,-2,0,0,-1,-2,-3,-2,2]
                  ,[-1,-1,-3,-3,0,-4,-3,4,-3,2,1,-3,-3,-3,-3,-2,-1,3,-3,-1]
                  ,[-1,-3,-1,1,-3,-2,-1,-3,5,-2,-1,0,-1,1,2,0,-1,-2,-3,-2]
                  ,[-1,-1,-4,-3,0,-4,-3,2,-2,4,2,-3,-3,-2,-2,-2,-1,1,-2,-1]
                  ,[-1,-1,-3,-2,0,-3,-2,1,-1,2,5,-2,-2,0,-1,-1,-1,1,-1,-1]
                  ,[-2,-3,1,0,-3,0,1,-3,0,-3,-2,6,-2,0,0,1,0,-3,-4,-2]
                  ,[-1,-3,-1,-1,-4,-2,-2,-3,-1,-3,-2,-2,7,-1,-2,-1,-1,-2,-4,-3]
                  ,[-1,-3,0,2,-3,-2,0,-3,1,-2,0,0,-1,5,1,0,-1,-2,-2,-1]
                  ,[-1,-3,-2,0,-3,-2,0,-3,2,-2,-1,0,-2,1,5,-1,-1,-3,-3,-2]
                  ,[1,-1,0,0,-2,0,-1,-2,0,-2,-1,1,-1,0,-1,4,1,-2,-3,-2]
                  ,[0,-1,-1,-1,-2,-2,-2,-1,-1,-1,-1,0,-1,-1,-1,1,5,0,-2,-2]
                  ,[0,-1,-3,-2,-1,-3,-3,3,-2,1,1,-3,-2,-2,-3,-2,0,4,-3,-1]
                  ,[-3,-2,-4,-3,1,-2,-2,-3,-3,-2,-1,-4,-4,-2,-3,-3,-2,-3,11,2]
                  ,[-2,-2,-3,-2,3,-3,2,-1,-2,-1,-1,-2,-3,-1,-2,-2,-2,-1,2,7]]

scsTable :: String -> String -> Array (Int,Int) (Int, Change)
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
    
    score :: (Int,Int) -> Int
    score (i,j) = Map.findWithDefault (error "invalid amino acid") (x ! i, y ! j) blosum62

    dist :: (Int, Int) -> (Int, Change)
    dist (0,0) = (0, Stop)
    dist (0,j) = (-5*j, Add)
    dist (i,0) = (-5*i, Remove)
    dist (i,j)
        | maxVal == l    = (l, Add)
        | maxVal == u    = (u, Remove)
        | maxVal == d    = (d, Modify)
        | otherwise      = error "dude, wtf"
        where l = tableVal (i, j-1) - 5
              u = tableVal (i-1, j) - 5
              d = tableVal (i-1, j-1) + score (i,j)
              maxVal = maximum [u,l,d]

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
          --print tab
          --let (x', y') = buildStrings xs ys . modifications $ tab 
          --putStrLn x'
          --putStrLn y'

