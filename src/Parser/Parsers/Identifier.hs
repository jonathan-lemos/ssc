module Parser.Parsers.Identifier where
import Parser.Parser (Parser)

identifier :: Parser String
identifier = consumeWhile 
