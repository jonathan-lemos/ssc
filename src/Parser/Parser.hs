module Parser.Parser where

import Control.Monad.Trans.State (State)
import Parser.Context (ParserContext (ParserContext), ParserSequence)
import Parser.Error (ParserError (ParserError))
import Parser.Utils (mapFirst)

newtype Parser t = Parser {parse :: ParserSequence -> Either ParserError (t, ParserSequence)}

instance Functor Parser where
  fmap f (Parser o) = Parser $ fmap (mapFirst f) . o

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
  

parseValue :: a -> ParserSequence -> Either ParserError (a, ParserSequence)
parseValue = (Right .) . (,)

parseError :: ParserContext -> String -> Either ParserError a
parseError ctx msg = Left $ ParserError ctx msg
