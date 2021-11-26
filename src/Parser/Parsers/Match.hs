module Parser.Parsers.Match where

import Parser.Parsers.Conditional
import Parser.Parser

match :: (Eq v, Show v) => Parser v -> v -> Parser v
match parser value =
  conditional
    (== value)
    (\actual -> concat ["Expected '", show value, "', but got '", show actual, "'"])
    parser