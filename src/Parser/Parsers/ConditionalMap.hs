{-# LANGUAGE TupleSections #-}
module Parser.Parsers.ConditionalMap where
import Parser.Error
import Parser.Parser
import Control.Monad

conditionalMap :: (a -> Either ParserError b) -> Parser a -> Parser b
conditionalMap f (Parser p) = Parser $ mapper <=< p where
    mapper (val, seq) = (, seq) <$> f val
