module Parser.Parsers.Peek where

import Parser.Parser
import Parser.Context
import Parser.Utils

peek :: Parser Char
peek = Parser f
  where
    f ((ch, _ctx) :<> xs) = parseValue ch $ (ch, _ctx) :<> xs
    f (EOF x) = parseError x "Expected a character, but there wasn't one to parse."
