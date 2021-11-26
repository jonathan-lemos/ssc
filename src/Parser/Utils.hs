module Parser.Utils where

import Parser.Parser
import Parser.Error
import Parser.Context
import Control.Applicative

-- | Creates a successful parse result from a value and a (remaining) sequence.
parseValue :: a -> ParserSequence -> Either ParserError (a, ParserSequence)
parseValue = (Right .) . (,)

-- | Creates a parse error from a HasContext and an error message.
parseError :: (HasContext c) => c -> String -> Either ParserError a
parseError ctx msg = Left $ ParserError (getContext ctx) msg

-- | Creates a Parser that always errors with the given message.
reject :: String -> Parser a
reject msg = Parser $ \seq -> Left ParserError { message = msg, errContext = getContext seq }

maybeReject :: Maybe a -> String -> Parser a
maybeReject mb msg =
  case mb of
    Just v -> pure v
    Nothing -> reject msg

eitherReject :: Either String a -> Parser a
eitherReject e =
  case e of
    Left msg -> reject msg
    Right v -> pure v
