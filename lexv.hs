import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord 

strs :: [String] -> Int -> [String]
strs alph 0 = [""]
strs alph n = do c <- alph
                 rest <- strs alph (n-1)
                 return (c ++ rest)

order :: String -> Char -> Int
order alph x = fromJust . findIndex (==x) $ alph

orderS :: String -> String -> String -> Ordering
orderS _ []     []     = EQ
orderS _ []     (_:_)  = LT
orderS _ (_:_)  []     = GT
orderS alph (x:xs) (y:ys) = case (comparing $ order alph) x y of
                                EQ    -> orderS alph xs ys
                                other -> other

main :: IO ()
main = do alph <- words <$> getLine
          n    <- read <$> getLine
          let alph' = concat alph
          mapM_ putStrLn . sortBy (orderS alph') . concatMap (strs alph) $ [1..n]
