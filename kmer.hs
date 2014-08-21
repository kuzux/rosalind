{-# LANGUAGE TupleSections #-}

import Fasta
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Functor
import Data.List

type FMer = (Char,Char,Char,Char)

fourMers :: String -> [FMer]
fourMers xs = zip4 xs (tail xs) (tail . tail $ xs) (tail . tail . tail $ xs)

initMap :: Map FMer Int
initMap = M.fromList $ (,0) <$> fours
    where fours = [(x,y,z,t) | x <- xs, y <- xs, z <- xs, t <- xs]
          xs    = "ACGT"

countFMers :: [FMer] -> Map FMer Int
countFMers = foldl' f initMap
    where f map fmer = M.update (Just . succ) fmer map

main :: IO ()
main = putStrLn =<< unwords . map (show . snd) . M.toAscList . countFMers . fourMers . head . parseAsStrings <$> getContents
