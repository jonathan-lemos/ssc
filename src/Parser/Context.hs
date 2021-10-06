module Parser.Context(fromStdinLine, fromFile) where

data ParserSource
  = FileSource
      { filename :: String,
        lineNo :: Int
      }
  | StdinSource

data ParserContext = ParserContext
  { charNo :: Int,
    current :: Char,
    currentLine :: String,
    source :: ParserSource
  }

-- |Makes a `ParserContext` list from a `ParserSource` and a line of text.
--  A ParserContext is made for each Char in the line.
fromLine :: ParserSource -> String -> [ParserContext]
fromLine src str = go 1 str where
  go i (x:xs) = ParserContext i x str src : go (i + 1) xs
  go i [] = []

-- |Makes a `ParserContext` list from a line from standard input.
--  A ParserContext is made for each Char in the input.
fromStdinLine :: String -> [ParserContext]
fromStdinLine = fromLine StdinSource

-- |Makes a `ParserContext` list from a list of lines from a file.
-- Helper function for `fromFile`
fromLines :: String -> [String] -> [ParserContext]
fromLines filename list = concat $ go 1 list where
  go i (x:xs) = fromLine (FileSource filename i) x : go (i + 1) xs
  go _ [] = []


-- |Makes a `ParserContext` from a filename.
fromFile :: String -> IO [ParserContext]
fromFile filename = do
  contents <- readFile filename
  return . fromLines filename $ lines contents
