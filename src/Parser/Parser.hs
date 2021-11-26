module Parser.Parser where

import Parser.Context
import Parser.Error
import Utils (mapFirst)
import Control.Monad
import Data.Default
import Control.Applicative (Alternative)

-- | A function that takes in a sequence of tokens and returns a value and a new sequence (after the parsed section), or an error.
newtype Parser t = Parser {parse :: ParserSequence -> Either ParserError (t, ParserSequence)}

instance Functor Parser where
  fmap f (Parser o) = Parser $ fmap (mapFirst f) . o

instance Applicative Parser where
  pure = Parser . (Right .) . (,)

  (Parser ab) <*> (Parser a) =
    Parser $ transform <=< ab
    where
      transform (f, seq) = mapFirst f <$> a seq

instance Monad Parser where
  (Parser f) >>= g =
    Parser $ transform <=< f
    where
      transform (a, seq) = (parse $ g a) seq
