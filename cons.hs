import Data.List
import Data.Functor
import Fasta

process :: [String] -> [(Int,Int,Int,Int)]
process =  map countStuff . transpose
    where countStuff xs = (count 'A' xs, count 'C' xs, count 'G' xs, count 'T' xs)
          count x       = length . (elemIndices x)

consensus :: [(Int,Int,Int,Int)] -> String
consensus = map consensusC
    where consensusC (a,c,g,t) | maximum [a,c,g,t] == a = 'A'
                               | maximum [a,c,g,t] == c = 'C'
                               | maximum [a,c,g,t] == g = 'G'
                               | maximum [a,c,g,t] == t = 'T'

prettify :: [(Int,Int,Int,Int)] -> String
prettify xs = unlines $ [nums "A: " _1, nums "C: " _2, nums "G: " _3, nums "T: " _4]
    where _1 (a,_,_,_) = a
          _2 (_,a,_,_) = a
          _3 (_,_,a,_) = a
          _4 (_,_,_,a) = a
          nums header f = (header++) . unwords . map (show . f) $ xs

main :: IO ()
main = do
    mat      <- parseAsStrings <$> getContents
    let res  = process mat
    let cons = consensus res
    putStrLn cons
    putStrLn . prettify $ res
