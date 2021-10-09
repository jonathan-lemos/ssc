module Parser.Context where

-- | Describes the origin of the characters the parser is iterating over.
data ParserSource
  = FileSource
      { filename :: String,
        lineNo :: Int
      }
  | StdinSource
  deriving (Eq, Show)

-- | Describes a position in the sequence of characters the parser is iterating over.
data ParserContext = ParserContext
  { charNo :: Int,
    currentLine :: String,
    source :: ParserSource
  }
  deriving (Eq, Show)

-- | A sequence of positions the parser is iterating over (one per character).
data ParserSequence = (Char, ParserContext) :<> ParserSequence | EOF ParserContext

instance Semigroup ParserSequence where
  (<>) (x :<> xs) b = x :<> (xs <> b)
  (<>) (EOF _) b = b

-- | Makes a `ParserSequence` from a `ParserSource` and a line of text.
--   A ParserContext is made for each Char in the line.
--   If you do not have a `ParserSource`, use `fromStdinLine` or `fromFile` instead.
fromLine :: ParserSource -> String -> ParserSequence
fromLine src str = go 1 str
  where
    ctx i = ParserContext {charNo = i, currentLine = str, source = src}
    go i (x : xs) = (x, ctx i) :<> go (i + 1) xs
    go i [] = EOF $ ctx i

-- | Makes a `ParserSequence` from a line from standard input.
fromStdinLine :: String -> ParserSequence
fromStdinLine = fromLine StdinSource

-- | Makes a `ParserSequence` from a filename and a list of lines.
--  Use `fromFile` instead unless you already have the lines.
fromLines :: String -> [String] -> ParserSequence
fromLines filename = go 1
  where
    source i = FileSource {filename = filename, lineNo = i}
    go i [x] = fromLine (source i) x
    go i (x : xs) = fromLine (source i) x <> go (i + 1) xs
    go _ [] = EOF ParserContext {charNo = 1, currentLine = "", source = source 1}

-- | Splits a string into lines, keeping the newline character at the end of each line.
linesWithNewlineChar :: String -> [String]
linesWithNewlineChar s = f s "" []
  where
    f ('\n' : xs) buf ret = f xs "" (buf : ret)
    f (x : xs) buf ret = f xs (x : buf) ret
    f [] buf ret = reverse ret

-- | Makes a `ParserSequence` from a filename.
fromFile :: String -> IO ParserSequence
fromFile filename = do
  contents <- readFile filename
  return . fromLines filename $ linesWithNewlineChar contents
