module Parser.Parsers.Char where

import Parser.Context
import Parser.Parser
import Parser.Utils
import Parser.Parsers.Conditional

char :: Parser Char
char = Parser f
  where
    f ((ch, _ctx) :<> xs) = parseValue ch xs
    f (EOF x) = parseError x "Expected a character, but there wasn't one to parse."
    
matchChar :: Char -> Parser Char
matchChar c =
  conditional
    (== c)
    