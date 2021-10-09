module Parser.Context.Utils where
import Parser.Context

-- |Converts a ParserSequence into the equivalent list and EOF.
toListAndEOF :: ParserSequence -> ([(Char, ParserContext)], ParserContext)
toListAndEOF = go [] where
  go buf (x :<> xs) = go (x : buf) xs
  go buf (EOF x) = (buf, x)

-- |Converts a ParserSequence into the equivalent list. It does not include the EOF.
toList :: ParserSequence -> [(Char, ParserContext)]
toList = fst . toListAndEOF

foldrWhile :: (Char -> ParserContext -> a -> a) -> ParserSequence