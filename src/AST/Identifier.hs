module AST.Identifier where

import Data.Char (isAlpha, isAlphaNum)
import Parser.Parser
import Parser.Parsers.Conditional
import Parser.Parsers.Token (token)

isIdentifier :: String -> Bool
isIdentifier (x : xs) = (x == '_' || isAlpha x) && all isAlphaNum xs
isIdentifier [] = False

identifier :: Parser String
identifier =
  conditional
    isIdentifier
    (\s -> concat ["Expected an identifier, but '", s, "' is not a valid identifier."])
    token
