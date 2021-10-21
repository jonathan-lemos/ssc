module Parser.Parsers.Conditional where
import Parser.Parser

conditional :: String -> (a -> Bool) -> Parser a -> Parser a
conditional msg validator = conditionalMap mapper where
    mapper = 