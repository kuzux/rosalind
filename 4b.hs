{-# LANGUAGE OverloadedStrings #-}

import Data.Functor
import Data.Monoid
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

overlap :: Text -> Text -> Bool
overlap xs ys = T.tail xs == T.init ys

overlapGraph :: [Text] -> [(Text,Text)]
overlapGraph kmers = [ (x,y) | x <- kmers, y <- kmers, x/=y, overlap x y ]

main :: IO ()
main = mapM_ (TIO.putStrLn . disp) =<< overlapGraph . T.lines <$> TIO.getContents
    where disp (x,y) = x <> " -> " <> y

