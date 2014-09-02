import Data.Array (Array, (!))
import qualified Data.Array as A

weights :: [Integer]
weights = [57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186]

countWays :: Integer -> Integer
countWays n = dyn ! n
    where dyn :: Array Integer Integer
          dyn = A.array (0,n) [ (i, go i) | i <- [0..n] ]

          go :: Integer -> Integer
          go 0 = 1
          go m = sum $ [dyn ! (m-k) | k <- weights, k <= m]

