module Parser.Parsers.TakeWhile where

takeWhile :: (Char -> Bool) -> Parser String

takeWhile f = Parser \ctx ->
    