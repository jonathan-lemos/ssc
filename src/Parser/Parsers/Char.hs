module Parser.Parsers.Char where

import Control.Monad.Trans.State.Lazy (state)
import Parser.Context
import Parser.Error
import Parser.Parser

char :: Parser Char
char = Parser f
  where
    f ((ch, ctx) :<> xs) = parseValue ch xs
    f (EOF x) = parseError x "Expected a character, but there wasn't one to parse."