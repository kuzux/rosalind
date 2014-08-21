import Control.Applicative
import Data.Array (Array, (!), array, range, bounds, assocs)
import Data.List
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Fasta

data Change = Add | Remove | Modify | Stop deriving (Eq, Show)

pam250 :: Map (Char, Char) Int
pam250 = Map.fromList . zip ((,) <$> prots <*> prots) . concat $ table
    where prots = ['A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y']
          table = [[2,-2,0,0,-3,1,-1,-1,-1,-2,-1,0,1,0,-2,1,1,0,-6,-3]
                  ,[-2,12,-5,-5,-4,-3,-3,-2,-5,-6,-5,-4,-3,-5,-4,0,-2,-2,-8,0]
                  ,[0,-5,4,3,-6,1,1,-2,0,-4,-3,2,-1,2,-1,0,0,-2,-7,-4]
                  ,[0,-5,3,4,-5,0,1,-2,0,-3,-2,1,-1,2,-1,0,0,-2,-7,-4]
                  ,[-3,-4,-6,-5,9,-5,-2,1,-5,2,0,-3,-5,-5,-4,-3,-3,-1,0,7]
                  ,[1,-3,1,0,-5,5,-2,-3,-2,-4,-3,0,0,-1,-3,1,0,-1,-7,-5]
                  ,[-1,-3,1,1,-2,-2,6,-2,0,-2,-2,2,0,3,2,-1,-1,-2,-3,0]
                  ,[-1,-2,-2,-2,1,-3,-2,5,-2,2,2,-2,-2,-2,-2,-1,0,4,-5,-1]
                  ,[-1,-5,0,0,-5,-2,0,-2,5,-3,0,1,-1,1,3,0,0,-2,-3,-4]
                  ,[-2,-6,-4,-3,2,-4,-2,2,-3,6,4,-3,-3,-2,-3,-3,-2,2,-2,-1]
                  ,[-1,-5,-3,-2,0,-3,-2,2,0,4,6,-2,-2,-1,0,-2,-1,2,-4,-2]
                  ,[0,-4,2,1,-3,0,2,-2,1,-3,-2,2,0,1,0,1,0,-2,-4,-2]
                  ,[1,-3,-1,-1,-5,0,0,-2,-1,-3,-2,0,6,0,0,1,0,-1,-6,-5]
                  ,[0,-5,2,2,-5,-1,3,-2,1,-2,-1,1,0,4,1,-1,-1,-2,-5,-4]
                  ,[-2,-4,-1,-1,-4,-3,2,-2,3,-3,0,0,0,1,6,0,-1,-2,2,-4]
                  ,[1,0,0,0,-3,1,-1,-1,0,-3,-2,1,1,-1,0,2,1,-1,-2,-3]
                  ,[1,-2,0,0,-3,0,-1,0,0,-2,-1,0,0,-1,-1,1,3,0,-5,-3]
                  ,[0,-2,-2,-2,-1,-1,-2,4,-2,2,2,-2,-1,-2,-2,-1,0,4,-6,-2]
                  ,[-6,-8,-7,-7,0,-7,-3,-5,-3,-2,-4,-4,-6,-5,2,-2,-5,-6,17,0]
                  ,[-3,0,-4,-4,7,-5,0,-1,-4,-1,-2,-2,-5,-4,-4,-3,-3,-2,0,10]]


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
    score (i,j) = Map.findWithDefault (error "invalid amino acid") (x ! i, y ! j) pam250

    dist :: (Int, Int) -> (Int, Change)
    dist (0,0) = (0, Stop)
    dist (0,j) = (0, Stop)
    dist (i,0) = (0, Stop)
    dist (i,j)
        | maxVal == l    = (l, Add)
        | maxVal == u    = (u, Remove)
        | maxVal == d    = (d, Modify)
        | otherwise      = (0, Stop)
        where l = tableVal (i, j-1) - 5
              u = tableVal (i-1, j) - 5
              d = tableVal (i-1, j-1) + score (i,j)
              maxVal = maximum [u,l,d, 0]

modifications :: (Int,Int) -> Array (Int,Int) (Int, Change) -> [Change]
modifications end table = go end
    where go ij@(i,j)
           | fst (table ! ij) <= 0 = []
           | otherwise             = case snd (table ! ij) of
                                        Stop   -> []
                                        Add    -> go (i,j-1) ++ [Add]
                                        Remove -> go (i-1,j) ++ [Remove]
                                        Modify -> go (i-1,j-1) ++ [Modify]

buildStrings :: String -> String -> [Change] -> (String, String)
buildStrings x y cs = fst3 $ foldl' go (("",""), x, y) cs
    where go ((xAcc, yAcc), xs, ys) c = case c of
              Modify -> ((xAcc ++ [head xs], yAcc ++ [head ys]), tail xs, tail ys)
              Add    -> ((xAcc, yAcc ++ [head ys]), xs, tail ys)
              Remove -> ((xAcc ++ [head xs], yAcc), tail xs, ys)
          fst3 (a,_,_) = a

maximumScore :: Array (Int, Int) (Int, Change) -> ((Int,Int), Int)
maximumScore table = (maxIdx, fst $ table ! maxIdx)
    where maxIdx = fst . maximumBy (comparing $ fst . snd) . assocs $ table

main :: IO ()
main = do (xs:ys:_) <- parseAsStrings <$> getContents
          let tab = scsTable xs ys
          let (end, score) = maximumScore tab
          print score
          --print tab
          let (x', y') = buildStrings xs ys . modifications end $ tab 
          putStrLn x'
          putStrLn y'

