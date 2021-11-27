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

-- | Conditionally returns a value or an error message depending on if the first argument is True or False.
conditionalReturn :: Bool -> a -> String -> Parser a
conditionalReturn True value _errMsg = pure value
conditionalReturn False _value errMsg = reject errMsg

-- | Returns the given value if the Maybe is Just, otherwise returns an error with the given message.
maybeReturn :: Maybe a -> String -> Parser a
maybeReturn mb msg =
  case mb of
    Just v -> pure v
    Nothing -> reject msg

-- | Returns the given value if the Either is Right, otherwise returns an error with the Left message.
eitherReturn :: Either String a -> Parser a
eitherReturn e =
  case e of
    Left msg -> reject msg
    Right v -> pure v
