import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString as B 

main :: IO ()
main = do needle <- B.getLine
          haystack <- B.getLine
          putStrLn . unwords . map show $ BSS.indices needle haystack
