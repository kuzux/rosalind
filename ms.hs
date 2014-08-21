import Data.Functor
import Data.List

main :: IO ()
main = do getLine
          xs <- sort . map read . words <$> getLine :: IO [Integer]
          putStrLn . unwords . map show $ xs

