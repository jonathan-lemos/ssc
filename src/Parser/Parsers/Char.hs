module Parser.Parsers.Char where

import Parser.Parser
import Parser.Context
import Control.Monad.Trans.State.Lazy (state)
import Parser.Error


func :: ParserSequence -> (Either ParserError Char, ParserSequence)
func ((ch, ctx) :<> xs) = (parse ch, xs)
func (EOF x) = parseError x "Expected a character, but there wasn't one to parse."

char :: Parser Char
char = state func
  where
    func ((ch, ctx) :<> xs) = (parse ch, xs)
    func (EOF x) = parseError x "Expected a character, but there wasn't one to parse."