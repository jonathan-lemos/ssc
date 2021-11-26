module Parser.Parsers.ConditionalMap where
import Parser.Error
import Parser.Parser
import Control.Monad
import Parser.Utils

conditionalMap :: (a -> Either String b) -> Parser a -> Parser b
conditionalMap f (Parser p) = Parser $ mapper <=< p where
    mapper (val, seq) = case f val of
      Left msg -> parseError seq msg
      Right v -> parseValue v seq
