import Data.Functor
import Data.List

quartets :: [(Char, String)] -> [(String,String,String,String)]
quartets xs = [(a,b,c,d) | a <- zeros, b <- zeros, c <- ones, d <- ones, a < b, c < d]
    where zero  = (=='0') . fst
          one   = (=='1') . fst
          zeros = map snd . filter zero $ xs
          ones  = map snd . filter one $ xs

main :: IO ()
main = do taxa <- words <$> getLine
          chars <- lines <$> getContents
          mapM_ (putStrLn . disp) . nubBy eq . concat . map quartets . map (\x -> zip x taxa) $ chars
    where disp (a,b,c,d) = "{" ++ a ++ ", " ++ b ++ "} {" ++ c ++ ", " ++ d ++ "}"
          eq t1@(a1,b1,c1,d1) t2@(a2,b2,c2,d2) = t1 == t2 || (a1 == c2 && b1 == d2 && c1 == a2 && d1 == b2)

