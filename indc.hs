import Text.Printf
import Data.Functor

fact :: Integer -> Integer
fact n = product [1..n]

comb :: Integer -> Integer -> Integer
comb n k = (fact n) `div` (fact k * fact (n-k))

sumCombs :: Integer -> Integer -> Integer
sumCombs m n = (sum $ map (comb n) [m..n]) `mod` 1000000

logProbs :: Integer -> [Double]
logProbs n = zipWith (+) (tail vars) (repeat const)
    where it a i = a - comb (2*n) i
          vars   = map (logBase 10 . fromIntegral) $ scanl it (2^(2*n)) [0..2*n-1]
          const  = (fromIntegral $ -2*n)*(logBase 10 2)

main :: IO ()
main = do n <- read <$> getLine
          putStrLn . unwords $ map (printf "%.4f") $ logProbs n
