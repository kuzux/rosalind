import Data.Functor

prob :: Double -> Double
prob p = 1 - p^2 - (1-p)^2

main :: IO ()
main = putStrLn =<< unwords . map (show . prob . read) . words <$> getLine

