module Parser.Parsers.ConsumeWhile (consumeWhile) where

import Control.Monad.Trans.State (State)
import Control.Monad.Trans.State.Lazy (state)
import GHC.Base (String)
import Parser.Context (ParserContext (ParserContext), current)
import Parser.Error (ParserError)
import Parser.Parser (Parser)

-- |A parser that consumes characters while the predicate is true.
consumeWhile :: (Char -> Bool) -> Parser String
consumeWhile f = state $ go []
  where
    go buf ctx =
      case ctx of
        (x : xs) ->
          if f (current x)
            then go xs (x : buf)
            else (Right $ fmap current buf, ctx)
        [] -> (Right $ fmap current buf, [])
