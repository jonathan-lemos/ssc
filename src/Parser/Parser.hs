module Parser.Parser where

import Control.Monad (join)
import Control.Monad.Trans.State (State)
import Parser.Context (ParserContext (ParserContext), ParserSequence)
import Parser.Error (ParserError (ParserError))

newtype Parser t = Parser {parse :: ParserSequence -> Either ParserError (t, ParserSequence)}

instance Functor Parser where
  fmap f (Parser o) = Parser $ fmap (mapTuple f) . o
    where
      mapTuple f (a, b) = (f a, b)

instance Applicative Parser where
  pure = Parser . (Right .) . (,)

  (Parser ab) <*> (Parser a) =
    Parser $ transform . ab
    where
      transform (Right (f, seq)) = mapTuple f <$> a seq
      transform (Left e) = Left e
      mapTuple f (a, b) = (f a, b)

instance Monad Parser where
  (Parser f) >>= g =
    Parser $ transform . f
    where
      transform (Right (a, seq)) = (parse $ g a) seq
      transform (Left e) = Left e
  

parseValue :: a -> Either ParserError a
parseValue = Right

parseError :: ParserContext -> String -> Either ParserError a
parseError ctx msg = Left $ ParserError ctx msg
