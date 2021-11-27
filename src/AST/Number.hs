module AST.Number (number) where

import Types.Scalar
import Data.Number.Fixed
import Parser.Parsers.Char
import Parser.Parser
import Parser.Parsers.Match
import Parser.Parsers.ConsumeWhile
import Parser.Parsers.FirstOf
import Parser.Parsers.ParsOr
import Parser.Utils
import Text.Read
import Data.Char

fromString :: (Read a) => String -> Parser a
fromString = eitherReturn . readEither

digits :: Parser String
digits = consumeWhileChar isDigit

sign :: Parser Char
sign = firstOfOr [match char '-', match char '+'] '+'

integer :: Parser Integer
integer = do
  s <- sign
  nums <- digits
  eitherReturn $ readEither (show s ++ nums)

power :: Parser Integer
power = do
  _ <- matchFirstOf char ['e', 'E']
  integer

float :: Parser (Fixed Prec50)
float = do
  whole <- integer
  _ <- match char '.'
  frac <- digits
  p <- power `parsOr` pure 0
  fromString $ concat [show whole, ".", show frac, "e", show p]

number :: Parser SscNumber
number = firstOf errorMapper [SscReal <$> float, SscInteger <$> expInteger, SscInteger <$> integer] where
  errorMapper = const "Expected a number."
  expInteger = do
    i <- integer
    p <- power
    fromString $ concat [show i, "e", show p]
