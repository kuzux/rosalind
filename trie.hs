import Data.List hiding (insert)
import Data.Functor
import Text.Printf

newtype Trie a = Trie [(a, Trie a)] deriving (Show, Eq)

insert :: (Eq a) => Trie a -> [a] -> Trie a
insert (Trie t) []     = Trie t
insert (Trie t) (x:xs) = case partition ((==x) . fst) t of
    ([], rest)      -> Trie $ rest ++ [(x, insert empty xs)]
    ([(_,f)], rest) -> Trie $ rest ++ [(x, insert f xs)]

empty :: Trie a
empty = Trie []

toEdgeList :: Int -> Trie a -> (Int, [(Int,Int,a)])
toEdgeList n (Trie t) = foldl' f (n+1, []) t
    where f (i,a) (x,t') = let (i', es) = toEdgeList i t' in (i', a ++ [(n,i,x)] ++ es)
      
trieStructure :: [String] -> [(Int,Int,Char)]
trieStructure = snd . toEdgeList 1 . foldl' insert empty

main :: IO ()
main = do edges <- trieStructure . lines <$> getContents
          mapM_ (disp) edges
    where disp (a,b,n) = printf "%d %d %c\n" a b n
