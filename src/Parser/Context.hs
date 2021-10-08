module Parser.Context where
import GHC.IO.Handle.FD (stdin)

-- |Describes the origin of the characters the parser is iterating over.
data ParserSource
  = FileSource
      { filename :: String,
        lineNo :: Int
      }
  | StdinSource

-- |Describes a position in the sequence of characters the parser is iterating over.
data ParserContext = ParserContext
  { charNo :: Int,
    currentLine :: String,
    source :: ParserSource
  }

-- |A sequence of positions the parser is iterating over (one per character).
data ParserSequence = (Char, ParserContext) :<> ParserSequence | EOF ParserContext

instance Semigroup ParserSequence where
  (<>) (x :<> xs) b = x :<> (xs <> b)
  (<>) (EOF _) b = b

instance Show ParserSequence where
  show = fmap fst . psToList

-- |Converts a ParserSequence into the equivalent list and EOF.
psToListAndEOF :: ParserSequence -> ([(Char, ParserContext)], ParserContext)
psToListAndEOF = go [] where
  go buf (x :<> xs) = go (x : buf) xs
  go buf (EOF x) = (buf, x)

-- |Converts a ParserSequence into the equivalent list. It does not include the EOF.
psToList :: ParserSequence -> [(Char, ParserContext)]
psToList = fst . psToListAndEOF

-- |Makes a `ParserSequence` from a `ParserSource` and a line of text.
--  A ParserContext is made for each Char in the line.
--  If you do not have a `ParserSource`, use `fromStdinLine` or `fromFile` instead.
fromLine :: ParserSource -> String -> ParserSequence
fromLine src str = go 1 str where
  ctx i = ParserContext { charNo = i, currentLine = str, source = src }
  go i (x:xs) = (x, ctx i) :<> go (i + 1) xs
  go i [] = EOF $ ctx i

-- |Makes a `ParserSequence` from a line from standard input.
fromStdinLine :: String -> ParserSequence
fromStdinLine = fromLine StdinSource

-- |Makes a `ParserSequence` from a filename and a list of lines.
-- Use `fromFile` instead unless you already have the lines.
fromLines :: String -> [String] -> ParserSequence
fromLines filename = go 1 where
  source i = FileSource { filename = filename, lineNo = i }
  go i [x] = fromLine (source i) x
  go i (x:xs) = fromLine (source i) x <> go (i + 1) xs
  go _ [] = EOF ParserContext { charNo = 1, currentLine = "", source = source 1}

-- |Makes a `ParserSequence` from a filename.
fromFile :: String -> IO ParserSequence
fromFile filename = do
  contents <- readFile filename
  return . fromLines filename $ lines contents
