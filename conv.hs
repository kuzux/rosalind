import Control.Applicative
import Text.Printf
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Ord

addWord :: (Ord a, Integral n) => Map a n -> a -> Map a n
addWord m x = M.insert x (succ val) m
    where val = M.findWithDefault 0 x m 

wordFreqs :: (Ord a) => [a] -> Map a Int
wordFreqs = foldl addWord M.empty

largestSpectralConvolution :: [Double] -> [Double] -> (String, Int)
largestSpectralConvolution xs ys = maximumBy (comparing snd) . M.assocs . wordFreqs . map (printf "%.5f") $ spectralConvolution
    where spectralConvolution = (-) <$> xs <*> ys

main :: IO ()
main = do xs <- map read . words <$> getLine
          ys <- map read . words <$> getLine
          let (s, n) = largestSpectralConvolution xs ys
          print n
          putStrLn s

