module Parser.Parsers.FirstOf where

import Control.Monad (join)
import Data.List (intersperse)
import Parser.Error
import Parser.Parser
import Parser.Utils

defaultErrorMapper :: [ParserError] -> String
defaultErrorMapper errors = join $ intersperse "," $ quote . message <$> errors
  where
    quote s = concat ["'", show s, "'"]

-- | Returns the result of the first parser that matches, or an error with the given message.
firstOf :: ([ParserError] -> String) -> [Parser a] -> Parser a
firstOf errMapper parsers = fob parsers []
  where
    fob [] errors = Parser $ \seq -> parseError seq (errMapper $ reverse errors)
    fob (x : xs) errors = Parser $ \seq ->
      case parse x seq of
        Left e -> parse (fob xs (e : errors)) seq
        Right (v, seq) -> Right (v, seq)

-- | Returns the result of the first parser that matches, or returns the given default value and doesn't advance the sequence.
firstOfOr :: [Parser a] -> a -> Parser a
firstOfOr parsers def = firstOf defaultErrorMapper (parsers ++ [pure def])
