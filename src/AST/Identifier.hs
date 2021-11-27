module AST.Identifier where

import Data.Char (isAlpha, isAlphaNum)
import Parser.Parser
import Parser.Parsers.Char
import Parser.Parsers.ConsumeWhile
import Parser.Utils

identifier :: Parser String
identifier = do
  first <- char
  rest <- consumeWhileChar isAlphaNum
  if first /= '_' && not (isAlpha first) then
    reject $ concat ["Identifiers must start with _ or a-zA-Z (read '", first, "')"]
  else
    return $ first : rest
