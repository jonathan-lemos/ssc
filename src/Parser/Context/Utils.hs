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

foldWhile :: (Char -> ParserContext -> b -> Either a b) -> b -> ParserSequence -> (Maybe a, ParserSequence)
foldWhile f val ((ch, ctx) :<> xs) =
    case f ch ctx val of
        Left a -> (Just a, (ch, ctx) :<> xs)
        Right b -> foldWhile f b xs
foldWhile f val (EOF e) = (Nothing, EOF e) 

accumulateWhile :: (Char -> ParserContext -> a -> Either a a) -> a -> ParserSequence -> (a, ParserSequence)
accumulateWhile f val ((ch, ctx) :<> xs) =
    case f ch ctx val of
        Left a -> (a, (ch, ctx) :<> xs)
        Right b -> accumulateWhile f b xs
accumulateWhile f val (EOF e) = (val, EOF e)