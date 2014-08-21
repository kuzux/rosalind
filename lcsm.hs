{-# LANGUAGE OverloadedStrings #-}

import Data.Functor
import Data.List
import Data.Ord
import qualified Data.Text as Text
import Data.Text (Text)
import Fasta

{-
proc :: [Text] -> Seq Text -> Text
proc xs q 
    | all (Text.isInfixOf h) xs = h
    | otherwise                 = proc xs $ t |> Text.tail h |> Text.init h
    where (h :< t) = Seq.viewl q
-}

longestSS :: [Text] -> Text -> Text
longestSS xs st 
    | not $ all (Text.isInfixOf st) xs = Text.init st
    | otherwise                        = maximumBy (comparing Text.length) cands
    where cands = map (longestSS xs . Text.snoc st) "ACGT"

main :: IO ()
main = do xs <- map snd . parseFastas <$> getContents
          putStrLn . Text.unpack $ longestSS xs ""
