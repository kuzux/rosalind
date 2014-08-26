import Data.Functor
import Data.List

main :: IO ()
main = do _ <- getLine
          xs <- map read . words <$> getLine :: IO [Int]
          putStrLn . unwords . map show . sort $ xs

