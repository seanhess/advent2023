module App.Parse
  ( parseIO
  , parseFile
  , Parser
  , decimal
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude

type Parser = Parsec Void String

parseIO :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s a -> String -> s -> IO a
parseIO parser src inp =
  case parse parser src inp of
    Left bundle -> do
      putStrLn (errorBundlePretty bundle)
      fail "Failed Parse"
    Right g -> pure g

parseFile :: FilePath -> Parser a -> IO a
parseFile file prs = do
  inp <- readFile file
  parseIO prs file inp
