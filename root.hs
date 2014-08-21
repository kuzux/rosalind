import Data.Array (Array, (!), array)

numTrees :: Int -> Integer
numTrees n = dyn ! n
    where dyn :: Array Int Integer
          dyn = array (1, n) [(i, go i) | i <- [1..n]]

          go :: Int -> Integer
          go 1 = 1
          go n = sum [ (dyn ! i)*(dyn ! (n-i)) | i <- [1..n-1] ]

