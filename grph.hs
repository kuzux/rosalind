import qualified Data.Text as T
import Fasta
import Data.Functor

o3 :: T.Text -> T.Text -> Bool
o3 a b = T.takeEnd 3 a == T.take 3 b

edges :: [(T.Text,T.Text)] -> [(T.Text,T.Text)]
edges xs = [ (fst x, fst y) | x <- xs, y <- xs, fst x /= fst y, o3 (snd x) (snd y) ]

main :: IO ()
main = mapM_ putStrLn =<< map prettify . edges . parseFastas <$> getContents
    where prettify (a,b) = T.unpack a ++ " " ++ T.unpack b

