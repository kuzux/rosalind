module Fasta where

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Char
import Data.Either

fasta :: A.Parser (T.Text,T.Text)
fasta = do A.char '>'
           title <- A.takeWhile1 (not . isSpace) 
           A.skipSpace
           cont <- A.takeTill (== '>')
           return (title, T.filter (not . isSpace) cont)

fastas :: A.Parser [(T.Text,T.Text)]
fastas = fasta `AC.sepBy` A.skipSpace

parseFastas :: String -> [(T.Text, T.Text)]
parseFastas = either (const []) id . (A.parseOnly fastas) . T.pack

parseAsStrings :: String -> [String]
parseAsStrings = either (const []) (map $ T.unpack . snd) . (A.parseOnly fastas) . T.pack

