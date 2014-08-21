import Control.Applicative
import Data.List
import qualified Data.Text as T
import Fasta
import Protein


main :: IO ()
main = do dna <- snd . head . parseFastas <$> getContents
          let possible = concat $ translate <$> ([id, T.tail, T.drop 2] <*> [tToU dna, reverseComplement dna])
          let final = nub . filter (not . T.null) $ possible
          mapM_ (putStrLn . T.unpack) final
