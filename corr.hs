import Data.Functor
import Data.List
import Fasta

hammingDistance :: String -> String -> Int
hammingDistance x y = sum $ zipWith (\a b->if a==b then 0 else 1) x y

reverseComplement :: String -> String
reverseComplement = reverse . map comp
    where comp 'A' = 'T'
          comp 'C' = 'G'
          comp 'G' = 'C'
          comp 'T' = 'A'

separateCorrects :: [String] -> ([String], [String])  -- (corrects, incorrects)
separateCorrects = go ([],[]) 
    where go acc [] = acc
          go (corr,incor) (x:xs) = 
             case match of
                  [] -> go (corr, x:incor) xs
                  _  -> go ((reverseComplement x):x:corr, incor) nomatch
             where (match,nomatch) = partition fits xs
                   fits y = (hammingDistance x y == 0) || (hammingDistance x (reverseComplement y) == 0)

findMatches :: [String] -> [String] -> [(String, String)]
findMatches corr xs = zip xs (map match xs)
    where match x = head . filter (fits1 x) $ corr
          fits1 x y = hammingDistance x y == 1

main :: IO ()
main = do reads <- parseAsStrings <$> getContents
          let (corr, incor) = separateCorrects reads
          mapM_ putStrLn . map disp $ findMatches corr incor
       where disp (x,y) = x ++ "->" ++ y

