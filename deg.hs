import Control.Monad
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M

nums :: [Int] -> Map Int Int
nums xs = foldl add M.empty xs
    where add acc x = M.insert x (succ val) acc
            where val = M.findWithDefault 0 x acc

main :: IO()
main = do (n:m:_) <- map read . words <$> getLine
          dict <- nums . concat <$> replicateM m (map read . words <$> getLine) 
          putStrLn . unwords . map (show . getVal dict) $ [1..n]
    where getVal m x = M.findWithDefault 0 x m
