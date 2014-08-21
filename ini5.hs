import Data.Functor

main :: IO ()
main = mapM_ putStrLn =<< map snd . filter (even . fst) . zip [1..] . lines <$> getContents
