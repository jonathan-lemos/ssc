module Parser.Utils where

import Parser.Parser
import Parser.Error
import Parser.Context


parseValue :: a -> ParserSequence -> Either ParserError (a, ParserSequence)
parseValue = (Right .) . (,)

parseError :: ParserContext -> String -> Either ParserError a
parseError ctx msg = Left $ ParserError ctx msg

reject :: String -> Parser a
reject msg = Parser $ \seq -> Left ParserError { message = msg, context = seqContext seq }
