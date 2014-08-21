import Data.Functor

prob :: String -> Double -> Double
prob xs gc = sum $ map (logBase 10 . pr) xs
    where pr x | x == 'A' || x == 'T' = (1-gc)/2
               | x == 'G' || x == 'C' = gc/2

main :: IO ()
main = do xs <- getLine
          gcs <- map read . words <$> getLine
          mapM_ (print . prob xs) gcs
