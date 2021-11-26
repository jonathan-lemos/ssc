module AST.Number where

import Types.Scalar
import Data.Number.Fixed
import Parser.Parsers.Char
import Parser.Parser
import Parser.Error
import Parser.Parsers.Match
import Parser.Parsers.ConsumeWhile
import Parser.Parsers.FirstOf
import Parser.Parsers.ConditionalMap
import Parser.Utils
import Text.Read
import Data.Char

parseInteger :: Parser SscNumber
parseInteger = do
  c <- firstOf [match char '-', match char '+', pure '+'] ""
  nums <- consumeWhileChar isDigit
  eitherReject $ readEither (show c ++ nums)


parseFloat :: String -> Maybe (Fixed Prec50)
parseFloat = SscReal (readMaybe :: Integer)

