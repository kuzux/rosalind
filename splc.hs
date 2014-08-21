import Fasta
import Protein
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Functor
import Control.Monad

strip' :: Text -> Text -> [Text] -> Text
strip' acc curr is
    | T.null curr = acc
    | otherwise   = case msum . map (flip T.stripPrefix curr) $ is of
        Nothing   -> strip' (acc `T.snoc` T.head curr) (T.tail curr) is
        Just rest -> strip' acc rest is

strip :: Text -> [Text] -> Text
strip = strip' T.empty

main :: IO ()
main = do (s:is) <- map snd . parseFastas <$> getContents
          TIO.putStrLn . translate . tToU $ strip s is
          
