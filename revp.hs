import Data.Functor
import Fasta

revComplement :: String -> String
revComplement = reverse . map c
    where c 'A' = 'T'
          c 'T' = 'A'
          c 'G' = 'C'
          c 'C' = 'G'

isRevPalindrome :: String -> Bool
isRevPalindrome x = x == revComplement x

proc :: ([(Int,Int)],Int) -> String -> [(Int,Int)]
proc (acc,_) [] = acc
proc (acc,i) xs = proc (acc', i+1) (tail xs)
    where first n = (take n xs, n)
          toAdd = map (\(_,l) -> (i,l)) . filter (isRevPalindrome . fst) . map first . filter makesSense $ [4,6..12]
          acc' = acc ++ toAdd
          makesSense x = x <= length xs

main :: IO ()
main = do x <- head . parseAsStrings <$> getContents
          mapM_ (putStrLn . disp) $ proc ([],1) x 
    where disp (a,b) = unwords . map show $ [a,b]
