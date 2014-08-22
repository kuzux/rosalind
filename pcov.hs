import Data.Functor
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

findNext :: [Text] -> Text -> Text
findNext xs x = fromJust $ find (\y -> T.tail x == T.init y) xs

construct :: [Text] -> [Text]
construct xs@(start:_) = start:(takeWhile (/=start) . tail $ iterate (findNext xs) start)

unify :: [Text] -> Text
unify = foldl' go T.empty 
    where go acc x = acc `T.snoc` (T.last x)

main :: IO ()
main = TIO.putStrLn =<< unify . construct . T.lines <$> TIO.getContents

