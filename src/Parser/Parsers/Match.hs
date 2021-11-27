module Parser.Parsers.Match where

import Data.Foldable (find)
import Data.Maybe
import Parser.Parser
import Parser.Parsers.Char
import Parser.Parsers.Conditional
import Parser.Utils
import Utils

match :: (Eq v, Show v) => Parser v -> v -> Parser v
match parser value =
  conditional
    (== value)
    (\actual -> concat ["Expected '", show value, "', but got '", show actual, "'"])
    parser

matchFirstOf :: (Eq v, Show v) => Parser v -> [v] -> Parser v
matchFirstOf parser values = do
  value <- parser
  eitherReturn $ maybeToEither (concat ["Expected one of ", show values, ", but read '", show value, "'"]) $ find (== value) values

matchFirstOfOr :: (Eq v) => Parser v -> [v] -> v -> Parser v
matchFirstOfOr parser values def = do
  value <- parser
  return $ fromMaybe def $ find (== value) values
