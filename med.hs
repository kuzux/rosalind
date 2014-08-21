import Data.Functor
import Data.List

main :: IO ()
main = do getLine
          xs <- sort . map read . words <$> getLine :: IO [Integer]
          y <- read <$> getLine
          print $ xs !! (y-1)

