import Fasta
import qualified Data.Text as T
import Data.Functor
import Data.List

dist :: T.Text -> T.Text -> Double
dist a b = (fromIntegral diffs)/(fromIntegral len)
    where len = T.length a
          diffs = sum . map f $ T.zip a b
          f (x,y) | x == y    = 0
                  | otherwise = 1

calcMatrix :: [T.Text] -> [[Double]]
calcMatrix ts = [ [ dist a b | b <- ts ] | a <- ts ]

main :: IO ()
main = mapM_ putStrLn =<< (map $ unwords . map show) . calcMatrix . map snd . parseFastas <$> getContents 
