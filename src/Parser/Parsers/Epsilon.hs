module Parser.Parsers.Epsilon where

import Parser.Parser
import Parser.Utils

epsilon :: Parser ()
epsilon = Parser $ \seq -> parseValue () seq
