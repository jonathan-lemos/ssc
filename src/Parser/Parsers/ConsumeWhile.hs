module Parser.Parsers.ConsumeWhile (consumeWhile) where

import Control.Monad.Trans.State (State)
import Control.Monad.Trans.State.Lazy (state)
import GHC.Base (String)
import Parser.Context
import Parser.Error
import Parser.Parser

-- | A parser that consumes characters while the predicate is true.
consumeWhile :: (Char -> Bool) -> Parser String
consumeWhile pred = Parser (f "")
  where
    f buf seq = case seq of
      ((ch, ctx) :<> xs) ->
        if pred ch
          then f (ch : buf) xs
          else parseValue buf seq
      EOF ctx -> parseValue buf seq

-- | A parser that consumes characters while the predicate is true.
consumeWhileString :: (String -> Bool) -> Parser String
consumeWhileString pred = Parser (f "")
  where
    f buf seq = case seq of
      ((ch, ctx) :<> xs) ->
        if pred (ch : buf)
          then f (ch : buf) xs
          else parseValue buf seq
      EOF ctx -> parseValue buf seq
