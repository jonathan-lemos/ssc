module Parser.Parsers.Conditional where

import Parser.Parser
import Parser.Utils

conditional :: (a -> Bool) -> (a -> String) -> Parser a -> Parser a
conditional predicate messageMapper parser = do
  v <- parser
  if predicate v then
    return v
  else
    reject $ messageMapper v
