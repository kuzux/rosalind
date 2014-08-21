import Data.Functor

prob :: String -> Double -> Double
prob xs gc = product $ map pr xs
    where pr x | x == 'A' || x == 'T' = (1-gc)/2
               | x == 'G' || x == 'C' = gc/2

atLeastOne :: Int -> Double -> Double
atLeastOne n p = 1 - (1-p)^n

main :: IO ()
main = do (n':gc':_) <- words <$> getLine
          let n  = read n'
          let gc = read gc'
          xs <- getLine
          print . atLeastOne n $ prob xs gc

