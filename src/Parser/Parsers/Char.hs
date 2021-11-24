module Parser.Parsers.Char where

import Parser.Context
import Parser.Parser
import Parser.Utils

char :: Parser Char
char = Parser f
  where
    f ((ch, _ctx) :<> xs) = parseValue ch xs
    f (EOF x) = parseError x "Expected a character, but there wasn't one to parse."