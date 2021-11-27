module Parser.Parsers.ParsOr where

import Parser.Parser
import Parser.Parsers.FirstOf
import Control.Monad (join)
import Data.List (intersperse)
import Parser.Error

parsOr :: (Show a) => Parser a -> Parser a -> Parser a
parsOr a b = firstOf defaultErrorMapper [a, b]

infixl 7 `parsOr`