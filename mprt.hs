{-# LANGUAGE TupleSections #-}

import Data.Functor
import Fasta
import Network.HTTP
import Network.Browser

-- matches against the n-glycosylation motif N{P}[ST]{P}
motifPrefix :: String -> Bool
motifPrefix xs = case take 4 xs of
    [a,b,c,d] -> a == 'N' && b /= 'P' && (c == 'S' || c == 'T') && d /= 'P'
    otherwise -> False

locations :: String -> [Int]
locations xs = loc' [] 1 xs
    where loc' acc _ []   = acc
          loc' acc n curr | motifPrefix curr = loc' (acc++[n]) (n+1) (tail curr)
                          | otherwise        = loc'  acc       (n+1) (tail curr)

getFasta :: String -> IO (String, String)
getFasta id = (id,) . head . parseAsStrings . rspBody . snd <$> browse (setDebugLog Nothing >> setAllowRedirects True >> request (getRequest requestUrl))
    where requestUrl = "http://www.uniprot.org/uniprot/" ++ id ++ ".fasta"

main :: IO ()
main = do ids <- lines <$> getContents
          prots <- mapM getFasta ids
          let res = filter (not . null . snd) $ map (\(a,b) -> (a, locations b)) prots
          mapM_ putStrLn $ map disp res
    where disp (a,b) = unlines [a, unwords . map show $ b]

