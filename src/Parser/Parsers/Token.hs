module Parser.Parsers.Token (token) where
  
import Parser.Parser
import Parser.Parsers.Whitespace (whitespace)
import Parser.Parsers.ConsumeWhile (consumeWhileChar)
import Data.Char (isSpace)

-- | A Parser that returns a continuous string, not including the surrounding whitespace.
token :: Parser String
token = do
  _ <- whitespace
  consumeWhileChar $ not . isSpace
