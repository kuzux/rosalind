import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Newick

distFromRoot :: Tree -> String -> Maybe Int
distFromRoot (Leaf s) x 
    | s == Just x = Just 0
    | otherwise   = Nothing
distFromRoot (Internal s xs) x
    | s == Just x = Just 0
    | otherwise   = succ <$> (msum $ map (flip distFromRoot $ x) xs)

calcDistance :: Tree -> String -> String -> Maybe Int
calcDistance (Leaf _) _ _ = Nothing
calcDistance r@(Internal s xs) x y 
    | s == Just x          = distFromRoot r y
    | s == Just y          = distFromRoot r x
    | not . null $ hasBoth = calcDistance (head hasBoth) x y
    | otherwise            = (+) <$> distX <*> distY
    where distX   = distFromRoot r x
          distY   = distFromRoot r y
          hasBoth = filter f xs
          f c     = distFromRoot c x /= Nothing && distFromRoot c y /= Nothing

main :: IO ()
main = do res <- map calc . chunksOf 2 . filter (not . T.null) . T.lines <$> TIO.getContents
          putStrLn . unwords . map (show . fromJust) $ res
    where calc (t:r:_) = let (x:y:_) = words . T.unpack $ r in calcDistance (either error id $ parseTree t) x y
