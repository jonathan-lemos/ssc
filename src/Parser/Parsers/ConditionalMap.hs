{-# LANGUAGE TupleSections #-}
module Parser.Parsers.ConditionalMap where
import Parser.Error
import Parser.Parser
import Parser.Utils
import Control.Monad

conditionalMap :: (a -> Either ParserError b) -> Parser a -> Parser b
conditionalMap f p = Parser $ mapper <=< parse p where
    mapper (val, seq) = (, seq) <$> f val
