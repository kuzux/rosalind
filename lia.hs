import Data.Functor

type Probs = (Double, Double,Double)

-- apparently, for any generation , the probability of heterozygotes is 0.5
-- therefore; p(AaBb) = 0.25

--initGenotype :: Probs
--initGenotype = (0,1,0)

--crossWithHetero :: Probs -> Probs
--crossWithHetero (p1, p2, p3) = (p1*0.5 + p2*0.25, p1*0.5 + p2*0.5 + p3*0.5, p2*0.25 + p3*0.5)

factorial :: Integer -> Integer
factorial n = product [1..n]

binomial :: Integer -> Double -> Integer -> Double
binomial n p k = comb * p**k' * (1-p)**(n'-k')
    where comb  = fact' n / (fact' k * fact' (n-k))
          n'    = fromIntegral n
          k'    = fromIntegral k
          fact' = fromIntegral . factorial

totalProb :: Integer -> Int -> Double
totalProb k n = 1 - (sum . take n $ map (binomial (2^k) 0.25) [0..])

main :: IO ()
main = do (k:n:_) <- map read . words <$> getLine
          print $ totalProb k (fromIntegral n)
