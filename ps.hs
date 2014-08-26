import Data.Functor
import Data.List

main :: IO ()
main = do _ <- getLine
          xs <- map read . words <$> getLine :: IO [Int]
          k <- read <$> getLine
          putStrLn . unwords . map show . take k . sort $ xs

