module Parser.Parsers.FirstOf where

import Parser.Parser
import Parser.Utils

-- | Returns the result of the first parser that matches, or an error with the given message.
firstOf :: [Parser a] -> String -> Parser a
firstOf [] errMsg = Parser $ \seq -> parseError seq errMsg
firstOf (x:xs) errMsg = Parser $ \seq ->
  case parse x seq of
    Left _ -> parse (firstOf xs errMsg) seq
    Right (v, seq) -> Right (v, seq)
