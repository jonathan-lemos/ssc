module Parser.Error(ParserError(ParserError), printError, context, message) where

import Parser.Context
import System.Console.ANSI
import UI.HSCurses.Curses (scrSize)

data ParserError = ParserError
  { context :: ParserContext,
    message :: String
  }

-- |Returns the location of the context, e.g. `"thing.scc:5:1"` or `"stdin:4"`
lineInfo :: ParserContext -> String
lineInfo ctx =
  let cno = show $ charNo ctx
   in case source ctx of
        FileSource filename lineNo -> concat [filename, ":", show lineNo, ":", cno]
        StdinSource -> "stdin:" ++ cno

-- |Prints the error message itself.
printErrorLine :: ParserError -> IO ()
printErrorLine e = do
  putStr $ lineInfo (context e) ++ ": "

  setSGR [SetColor Foreground Vivid Red]
  putStr "error"

  setSGR [Reset]
  putStrLn $ ": " ++ message e

-- |Prints a string pointing to the error on the line above, e.g. `"   ^"`
makePointingString :: Int -> String
makePointingString n = concat (replicate (n - 1) " ") ++ "^"

-- |Truncates a string to a length centering around an index.
truncToLengthWithIndex :: Int -> Int -> String -> String
truncToLengthWithIndex len idx string =
  let deficit = length string - len
      left = idx - (deficit `div` 2)
   in take len (drop left string)

-- |Prints the error line and a line pointing to the character of said error.
printErrorLocation :: ParserError -> IO ()
printErrorLocation e = do
  (_len, width) <- scrSize

  let col = charNo $ context e
  let printTrimmed = putStrLn . truncToLengthWithIndex width col

  setSGR [SetColor Foreground Dull White]
  printTrimmed $ currentLine (context e)

  setSGR [SetColor Foreground Vivid Blue]
  printTrimmed $ makePointingString col

  setSGR [Reset]

-- |Prints an error to stdout
printError :: ParserError -> IO ()
printError e = do
  printErrorLine e
  printErrorLocation e
