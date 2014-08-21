{-# LANGUAGE OverloadedStrings #-}

module Newick ( Tree(..) , parseTree ) where

import Control.Applicative
import Data.Attoparsec.Text (Parser, many1, letter, char, string, sepBy, skipSpace, parseOnly, endOfInput)
import Data.Text (Text)

data Tree = Leaf (Maybe String)
          | Internal (Maybe String) [Tree]
          deriving (Eq, Show)

name :: Parser (Maybe String)
name = (Just <$> many1 (letter <|> char '_')) <|> pure Nothing

leaf :: Parser Tree
leaf = Leaf <$> name

internal :: Parser Tree
internal = (flip Internal) <$> children <*> label
    where children = string "(" *> skipSpace *> (subtree `sepBy` comma) <* skipSpace <* ")"
          comma    = skipSpace *> string "," <* skipSpace
          label    = skipSpace *> name

subtree :: Parser Tree
subtree = internal <|> leaf

tree :: Parser Tree
tree = skipSpace *> subtree <* string ";" <* skipSpace

parseTree :: Text -> Either String Tree
parseTree = parseOnly (tree <* endOfInput)

