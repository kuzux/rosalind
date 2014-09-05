import Data.Functor
import Data.List

kmers :: Int -> String -> [String]
kmers k xs = go [] xs
    where go acc xs | length xs < k = acc
                    | otherwise     = go ((take k xs):acc) (tail xs)

main :: IO ()
main = do k <- read <$> getLine
          dna <- getLine
          mapM_ putStrLn . sort . nub . kmers k $ dna

