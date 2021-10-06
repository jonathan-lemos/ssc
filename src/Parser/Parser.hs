module Parser.Parser where

import Control.Monad.Trans.State (State)
import Parser.Error (ParserError)
import Parser.Context (ParserContext)

type Parser t = State [ParserContext] (Either ParserError t)
