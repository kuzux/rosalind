import qualified Data.Set as S
import Data.List.Split
import Data.Functor
import Data.List

parseSet :: String -> S.Set Int
parseSet = S.fromList . map read . splitOn "," . init . tail 

showSet :: S.Set Int -> String
showSet s = "{" ++ str s ++ "}"
    where str = intercalate ", " . map show . S.toList

main :: IO ()
main = do n <- read <$> getLine
          let u = S.fromList [1..n]
          s1 <- parseSet <$> getLine
          s2 <- parseSet <$> getLine
          mapM_ (putStrLn . showSet) $ [S.union s1 s2, S.intersection s1 s2, S.difference s1 s2, S.difference s2 s1, S.difference u s1, S.difference u s2]
