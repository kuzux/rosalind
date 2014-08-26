import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord

table :: [(Char, Double)]
table = [ ('A',   71.03711)
        , ('C',   103.00919)
        , ('D',   115.02694)
        , ('E',   129.04259)
        , ('F',   147.06841)
        , ('G',   57.02146)
        , ('H',   137.05891)
        , ('I',   113.08406)
        , ('K',   128.09496)
        , ('L',   113.08406)
        , ('M',   131.04049)
        , ('N',   114.04293)
        , ('P',   97.05276)
        , ('Q',   128.05858)
        , ('R',   156.10111)
        , ('S',   87.03203)
        , ('T',   101.04768)
        , ('V',   99.06841)
        , ('W',   186.07931)
        , ('Y',   163.06333 )]
 
findMatch :: Double -> Maybe Char
findMatch x = case filter match table of
    []    -> Nothing
    ((x,_):_) -> Just x
    where match (_,y) = abs (x - y) < 0.01

pairUp :: Double -> [Double] -> [(Double, Double)]
pairUp w xs = go [] xs
    where go acc []     = acc
          go acc (x:xs) = go ((x,y):acc) xs'
              where ((y:_), xs') = partition (\a -> abs (w - x - a) < 0.01) xs


findMatches :: [(Double,Double)] -> Maybe [Char]
findMatches [] = Just []
findMatches [_] = Just []
findMatches ((x,_):xs) = msum $ do
        (y,y') <- xs
        let ys = without (y,y') xs
        [ (:) <$> findMatch (y-x) <*> findMatches ((y,y'):ys), (:) <$> findMatch (y'-x) <*> findMatches ((y',y):ys) ]
    where without x xs = filter (/=x) xs

orderPairs :: Double -> [(Double,Double)] -> [(Double, Double)]
orderPairs min ps | fst p == min = p:ps'
                  | otherwise    = (snd p, fst p):ps'
    where ((p:_), ps') = partition (\(a,b) -> a == min || b == min) ps

main :: IO ()
main = do total <- read <$> getLine
          ws <- map read . lines <$> getContents
          let first = minimum ws
          let n = length ws
          print . fromJust . findMatches . orderPairs first . pairUp total $ ws         

