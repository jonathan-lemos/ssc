module Parser.Parsers.ConsumeWhile (consumeWhile, consumeWhileChar, consumeWhileString) where

import Parser.Context
import Parser.Parser
import Parser.Context.Utils

-- | A parser that consumes characters while the current character, `ParserContext`, and accumulated string satisfy the given predicate, returning all of the characters consumed.
-- The string passed to the predicate includes all characters processed *before* the current character.
consumeWhile :: (Char -> ParserContext -> String -> Bool) -> Parser String
consumeWhile predicate = Parser $ Right . f
  where
    folder char ctx string =
      if predicate char ctx string
        then Right (char : string)
        else Left string
    f = accumulateWhile folder ""

-- | A parser that consumes characters while the current character satisfies the given predicate, returning all of the characters consumed.
consumeWhileChar :: (Char -> Bool) -> Parser String
consumeWhileChar predicate = consumeWhile $ const . const . predicate

-- | A parser that consumes characters while the current accumulated string satisfies the given predicate, returning said string.
consumeWhileString :: (String -> Bool) -> Parser String
consumeWhileString predicate = consumeWhile $ const $ const predicate