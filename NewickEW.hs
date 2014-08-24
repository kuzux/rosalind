{-# LANGUAGE OverloadedStrings #-}

module NewickEW ( Tree(..) , parseTree ) where

import Control.Applicative
import Data.Attoparsec.Text (Parser, many1, letter, char, string, sepBy, scientific, skipSpace, parseOnly, endOfInput)
import Data.Text (Text)

data Tree = Leaf (Maybe String)
          | Internal (Maybe String) [(Tree, Int)]
          deriving (Eq, Show)

name :: Parser (Maybe String)
name = (Just <$> many1 (letter <|> char '_')) <|> pure Nothing

leaf :: Parser Tree
leaf = Leaf <$> name

internal :: Parser Tree
internal = (flip Internal) <$> children <*> label
    where children = string "(" *> skipSpace *> (branch `sepBy` comma) <* skipSpace <* ")"
          comma    = skipSpace *> string "," <* skipSpace
          label    = skipSpace *> name

subtree :: Parser Tree
subtree = internal <|> leaf

branch :: Parser (Tree, Int)
branch = (,) <$> subtree <*> (floor <$> (skipSpace *> string ":" *> skipSpace *> scientific))

tree :: Parser Tree
tree = skipSpace *> subtree <* string ";" <* skipSpace

parseTree :: Text -> Either String Tree
parseTree = parseOnly (tree <* endOfInput)

