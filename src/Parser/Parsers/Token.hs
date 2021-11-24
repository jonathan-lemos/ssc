module Parser.Parsers.Token (token) where
  
import Parser.Parser
import Parser.Parsers.ConsumeWhile (consumeWhileChar)
import Data.Char (isSpace)

token :: Parser String
token = consumeWhileChar $ not . isSpace
