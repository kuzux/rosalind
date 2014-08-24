import Data.Functor

binomial :: Int -> Int -> Double -> Double
binomial i n p = (fromIntegral $ comb n i)*p**(fromIntegral i)*(1-p)**(fromIntegral $ n - i)

comb :: Int -> Int -> Int
comb m k = (fact m) `div` (fact k * fact (m-k))
    where fact n = product [1..n]

nextGen :: Int -> [Double] -> [Double]
nextGen n prev = [ prob i | i <- [0..2*n] ]
    where prob i = sum [ p'*(binomial i (2*n) $ (fromIntegral m) / (fromIntegral $ 2*n)) | (m, p') <- zip [0..] prev ]

initGen :: Int -> Int -> [Double]
initGen n m = [ if i == m then 1 else 0 | i <- [0..2*n] ]

calculate :: Int -> Int -> Int -> Int -> Double
calculate n m g k = sum . take (2*n-k+1) $ iterate (nextGen n) (initGen n m) !! g

main :: IO ()
main = do (n:m:g:k:_) <- map read . words <$> getLine
          print $ calculate n m g k

