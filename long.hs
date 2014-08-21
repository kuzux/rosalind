import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fasta

overlapLength :: Text -> Text -> Int
overlapLength x y = maximum $ map overlap [1..n]
    where n = min (T.length x) (T.length y)
          overlap i | T.takeEnd i x == T.take i y = i
                    | otherwise                   = 0

graph :: [(Text, Text)] -> [(Text,Text,Int)]
graph xs = do (l1, s1) <- xs
              (l2, s2) <- xs
              let n = min (T.length s1) (T.length s2)
              let l = overlapLength s1 s2
              if l1 /= l2 && 2*l >= n
                 then [(l1, l2, l)]
                 else []

indegree :: [(Text,Text,Int)] -> Text -> Int
indegree es x = length $ filter f es
    where f (_,b,_) = b == x

reconstruct :: [(Text,Text)] -> [(Text,Text,Int)] -> Text
reconstruct genes grp = go T.empty (start, 0)
    where start = fst . head . filter ((==0) . indegree grp . fst) $ genes

          next x = listToMaybe $ filter (\(a,_,_) -> a == x) grp

          go acc (gene, n) = case next gene of 
              Nothing      -> acc <> toAdd
              Just (_,b,c) -> go (acc <> toAdd) (b,c)

              where toAdd = T.drop n . fromJust $ lookup gene genes

main :: IO ()
main = do gs <- parseFastas <$> getContents
          let grp = graph gs
          TIO.putStrLn $ reconstruct gs grp
