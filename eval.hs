import Data.Functor

prob :: String -> Double -> Double
prob xs gc = product $ map pr xs
    where pr x | x == 'A' || x == 'T' = (1-gc)/2
               | x == 'G' || x == 'C' = gc/2

main :: IO ()
main = do n <- read <$> getLine
          str <- getLine
          gcs <- map read . words <$> getLine
          let m = length str
          let ps = map (prob str) gcs
          putStrLn . unwords . map show $ (*) (fromIntegral $ n-m+1) <$> ps 
