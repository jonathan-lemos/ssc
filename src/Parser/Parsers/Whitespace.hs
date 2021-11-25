module Parser.Parsers.Whitespace where

import Parser.Parser
import Parser.Parsers.ConsumeWhile
import Data.Char (isSpace)

-- | A Parser that consumes any amount of whitespace
whitespace :: Parser String
whitespace = consumeWhileChar isSpace
