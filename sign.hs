import Data.List
import Control.Applicative

signs :: Int -> [[Int]]
signs 0 = [[]]
signs n = (:) <$> [-1,1] <*> (signs $ n-1)

numSignedPermutations :: Int -> Integer
numSignedPermutations n = 2^n * (fact $ fromIntegral n)
    where fact x = product [1..x]

signedPermutations :: Int -> [[Int]]
signedPermutations n = zipWith (*) <$> signs n <*> permutations [1..n]

main :: IO ()
main = do n <- read <$> getLine
          print . numSignedPermutations $ n
          let perms = signedPermutations n
          mapM_ putStrLn . map unwords . map (map show) $ perms

