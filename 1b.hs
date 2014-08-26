import Data.Functor

revComplement :: String -> String
revComplement = reverse . map comp
    where comp 'A' = 'T'
          comp 'C' = 'G'
          comp 'G' = 'C'
          comp 'T' = 'A'

main :: IO ()
main = putStrLn =<< revComplement <$> getLine

