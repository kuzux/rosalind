import Data.Functor
import qualified Data.Heap as H

main :: IO ()
main = do _ <- getLine
          heap <- H.fromList . map read . words <$> getLine :: IO (H.MaxHeap Int)
          putStrLn . unwords . map show . H.toList $ heap

