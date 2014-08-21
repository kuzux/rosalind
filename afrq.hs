import Data.Functor

calc :: Double -> Double
calc p = 1-(1-(sqrt p))^2

main :: IO () 
main = putStrLn =<< unwords . map (show . calc . read) . words <$> getLine
