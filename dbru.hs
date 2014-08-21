{-# LANGUAGE OverloadedStrings #-}

import Data.Functor
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

reverseComplement :: Text -> Text
reverseComplement = T.reverse . T.map comp
    where comp 'A' = 'T'
          comp 'T' = 'A'
          comp 'C' = 'G'
          comp 'G' = 'C'



main :: IO ()
main = do inp <- T.lines <$> TIO.getContents
          let elems = (S.fromList inp) `S.union` (S.fromList . map reverseComplement $ inp)
          let edges = S.map edge $ elems
          mapM_ (TIO.putStrLn . disp) $ S.toList edges
    where edge x = (T.init x, T.tail x)
          disp (a,b) = "(" <> a <> ", " <> b <> ")"
