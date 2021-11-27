module Parser.Parsers.Keyword where

import Parser.Parser
import Parser.Parsers.Char
import Parser.Utils

keyword :: String -> Parser String
keyword str = ms str
  where
    ms (x : xs) = do
      c <- char
      if x == c
        then ms xs
        else reject $ concat ["Failed to match '", str, "'. Expected '", show x, "', got '", show c, "'"]
    ms [] = return str
