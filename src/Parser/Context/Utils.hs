module Parser.Context.Utils where
import Parser.Context
import Parser.Utils

-- | Converts a ParserSequence into the equivalent list and EOF.
toListAndEOF :: ParserSequence -> ([(Char, ParserContext)], ParserContext)
toListAndEOF = go [] where
  go buf (x :<> xs) = go (x : buf) xs
  go buf (EOF x) = (buf, x)

-- | Converts a ParserSequence into the equivalent list. It does not include the EOF.
toList :: ParserSequence -> [(Char, ParserContext)]
toList = fst . toListAndEOF

-- | Folds the given `ParserSequence` while the supplied function returns a `Right` value.
--   The folding function takes the current character, parser context, and state, and returns `Either` an output value (`Left`) or a new state (`Right`)
foldWhile :: (Char -> ParserContext -> b -> Either a b) -> b -> ParserSequence -> (Either a b, ParserSequence)
foldWhile f val ((ch, ctx) :<> xs) =
    case f ch ctx val of
        Left a -> (Left a, (ch, ctx) :<> xs)
        Right b -> foldWhile f b xs
foldWhile f val (EOF e) = (Right val, EOF e)

accumulateWhile :: (Char -> ParserContext -> a -> Either a a) -> a -> ParserSequence -> (a, ParserSequence)
accumulateWhile f val seq = mapFirst unite $ foldWhile f val seq
