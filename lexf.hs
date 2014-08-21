import Data.Functor

strs :: [String] -> Int -> [String]
strs alph 0 = [""]
strs alph n = do c <- alph
                 rest <- strs alph (n-1)
                 return (c ++ rest)

main :: IO ()
main = do alph <- words <$> getLine
          n    <- read <$> getLine
          mapM_ putStrLn $ strs alph n
