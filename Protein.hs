{-# LANGUAGE OverloadedStrings #-}

module Protein where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)

data Codone = AA Char | Stop deriving (Show, Eq)

transcribe :: Text -> Text
transcribe = T.map t 
    where t 'A' = 'U'
          t 'C' = 'G'
          t 'G' = 'C'
          t 'T' = 'A'
          t 'U' = 'A'

translate' :: Text -> [Codone]
translate' = map toCodone . filter (\x -> T.length x == 3) . T.chunksOf 3 . tToU

translate :: Text -> [Text]
translate xs = map (T.pack . map fromAA . takeWhile (not . isStop) . hasStop) $ map (flip drop $ translate' xs) inds
    where inds = findIndices (isStart) . translate' $ xs
          isStop Stop = True
          isStop _    = False
          isStart (AA 'M') = True
          isStart _        = False
          hasStop cs | Stop `elem` cs = cs
                     | otherwise      = []
          fromAA (AA x) = x

reverseComplement :: Text -> Text
reverseComplement = T.reverse . transcribe

tToU :: Text -> Text
tToU = T.map f 
    where f 'T' = 'U'
          f x   = x

toCodone :: Text -> Codone
toCodone "UUU" = AA 'F'
toCodone "UUC" = AA 'F'      
toCodone "UUA" = AA 'L'      
toCodone "UUG" = AA 'L'      
toCodone "UCU" = AA 'S'      
toCodone "UCC" = AA 'S'      
toCodone "UCA" = AA 'S'      
toCodone "UCG" = AA 'S'      
toCodone "UAU" = AA 'Y'      
toCodone "UAC" = AA 'Y'      
toCodone "UAA" = Stop   
toCodone "UAG" = Stop   
toCodone "UGU" = AA 'C'      
toCodone "UGC" = AA 'C'      
toCodone "UGA" = Stop   
toCodone "UGG" = AA 'W'      
toCodone "CUU" = AA 'L'      
toCodone "CUC" = AA 'L'      
toCodone "CUA" = AA 'L'      
toCodone "CUG" = AA 'L'      
toCodone "CCU" = AA 'P'      
toCodone "CCC" = AA 'P'      
toCodone "CCA" = AA 'P'      
toCodone "CCG" = AA 'P'      
toCodone "CAU" = AA 'H'      
toCodone "CAC" = AA 'H'      
toCodone "CAA" = AA 'Q'      
toCodone "CAG" = AA 'Q'      
toCodone "CGU" = AA 'R'      
toCodone "CGC" = AA 'R'      
toCodone "CGA" = AA 'R'      
toCodone "CGG" = AA 'R'
toCodone "AUU" = AA 'I'      
toCodone "AUC" = AA 'I'      
toCodone "AUA" = AA 'I'      
toCodone "AUG" = AA 'M'      
toCodone "ACU" = AA 'T'      
toCodone "ACC" = AA 'T'      
toCodone "ACA" = AA 'T'      
toCodone "ACG" = AA 'T'      
toCodone "AAU" = AA 'N'      
toCodone "AAC" = AA 'N'      
toCodone "AAA" = AA 'K'      
toCodone "AAG" = AA 'K'      
toCodone "AGU" = AA 'S'      
toCodone "AGC" = AA 'S'      
toCodone "AGA" = AA 'R'      
toCodone "AGG" = AA 'R'            
toCodone "GUU" = AA 'V'
toCodone "GUC" = AA 'V'
toCodone "GUA" = AA 'V'
toCodone "GUG" = AA 'V'
toCodone "GCU" = AA 'A'
toCodone "GCC" = AA 'A'
toCodone "GCA" = AA 'A'
toCodone "GCG" = AA 'A'
toCodone "GAU" = AA 'D'
toCodone "GAC" = AA 'D'
toCodone "GAA" = AA 'E'
toCodone "GAG" = AA 'E'
toCodone "GGU" = AA 'G'
toCodone "GGC" = AA 'G'
toCodone "GGA" = AA 'G'
toCodone "GGG" = AA 'G'
toCodone _     = Stop
