{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Functor
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

invCodonTable :: [(Char, [String])]
invCodonTable = [ ('A', ["GCT","GCG","GCC","GCA"])
                , ('R', ["CGT","CGC","CGA","CGG","AGA","AGG"])
                , ('N', ["AAT","AAC"])
                , ('D', ["GAT","GAC"])
                , ('C', ["TGT","TGC"])
                , ('Q', ["CAA","CAG"])
                , ('E', ["GAA","GAG"])
                , ('G', ["GGT","GGG","GGC","GGA"])
                , ('H', ["CAT","CAC"])
                , ('I', ["ATT","ATC", "ATA"])
                , ('L', ["TTA","TTG","CTT","CTC","CTA","CTG"])
                , ('K', ["AAA","AAG"])
                , ('M', ["ATG"])
                , ('F', ["TTT","TTC"])
                , ('P', ["CCT","CCG","CCC","CCA"])
                , ('S', ["TCT","TCG","TCC","TCA","AGT","AGC"])
                , ('T', ["ACT","ACG","ACC","ACA"])
                , ('W', ["TGG"])
                , ('Y', ["TAT","TAC"])
                , ('V', ["GTT","GTG","GTC","GTA"]) ]

possibleEncodings :: String -> [[String]]
possibleEncodings [] = [[]]
possibleEncodings (x:xs) = (:) <$> (fromJust $ lookup x invCodonTable) <*> (possibleEncodings xs)

revComplement :: Text -> Text
revComplement = T.reverse . T.map c
    where c 'A' = 'T'
          c 'C' = 'G'
          c 'G' = 'C'
          c 'T' = 'A'


main :: IO ()
main = do haystack <- TIO.getLine
          encodings <- map (T.pack . concat) . possibleEncodings <$> getLine
          let all = nub $ encodings ++ map revComplement encodings
          mapM_ TIO.putStrLn . filter (`T.isInfixOf` haystack) $ all
