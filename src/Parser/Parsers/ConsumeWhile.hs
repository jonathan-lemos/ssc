module Parser.Parsers.ConsumeWhile (consumeWhile) where

import Control.Monad.Trans.State (State)
import Control.Monad.Trans.State.Lazy (state)
import GHC.Base (String)
import Parser.Context
import Parser.Error
import Parser.Parser
import Parser.Context.Utils

-- | A parser that consumes characters while the current character, `ParserContext`, and accumulated string satisfy the given predicate, returning all of the characters consumed.
-- The string passed to the predicate includes all characters processed *before* the current character.
consumeWhile :: (Char -> ParserContext -> String -> Bool) -> Parser String
consumeWhile pred = Parser $ Right . f
  where
    folder char ctx string =
      if pred char ctx string
        then Right (char : string)
        else Left string
    f = accumulateWhile folder ""

-- | A parser that consumes characters while the current character satisfies the given predicate, returning all of the characters consumed.
consumeWhileChar :: (Char -> Bool) -> Parser String
consumeWhileChar pred = consumeWhile $ const . const . pred

-- | A parser that consumes characters while the current character satisfies the given predicate, returning all of the characters consumed.
consumeWhileString :: (String -> Bool) -> Parser String
consumeWhileString pred = Parser $ Right . f
  where
    folder char _ctx string =
      if pred string
        then Right (char : string)
        else Left string
    f = accumulateWhile folder ""
