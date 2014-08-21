import Data.List
import Data.Functor

probs :: [Double]
probs = [1,1,1,0.75,0.5,0]

expOffsprings :: [Double] -> Double
expOffsprings = (*2) . sum . (zipWith (*) probs)

main :: IO ()
main = print =<< expOffsprings . map read . words <$> getLine
