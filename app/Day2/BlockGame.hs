module Day2.BlockGame where

import App.Prelude
import Data.List qualified as L
import Data.String.Interpolate (i)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Game = Game
  { gameId :: Int
  , handfuls :: [Blocks]
  }
  deriving (Show, Eq)

data Blocks = Blocks
  { red :: Int
  , green :: Int
  , blue :: Int
  }
  deriving (Show, Eq)

data Color = Red | Green | Blue
  deriving (Eq, Show)

data ColorBlocks = ColorBlocks {color :: Color, numBlocks :: Int}
  deriving (Show, Eq)

-- Parsers ------------------------------

type Parser = Parsec Void String

parseGames :: Parser [Game]
parseGames = do
  parseGame `endBy` newline

parseGame :: Parser Game
parseGame = do
  _ <- string "Game "
  gameId <- decimal
  _ <- string ": "
  bs <- parseBlocks `sepBy` char ';'
  pure $ Game gameId bs

parseBlocks :: Parser Blocks
parseBlocks = do
  cbs <- colorBlocks `sepBy` string ", "
  pure $ toBlocks cbs
 where
  colorBlocks :: Parser ColorBlocks
  colorBlocks = do
    space
    n <- decimal
    space
    c <- color
    pure $ ColorBlocks c n

  toBlocks :: [ColorBlocks] -> Blocks
  toBlocks cbs =
    let red = blocksByColor Red cbs
        green = blocksByColor Green cbs
        blue = blocksByColor Blue cbs
     in Blocks{red, green, blue}

  color :: Parser Color
  color =
    (Green <$ string "green")
      <|> (Red <$ string "red")
      <|> (Blue <$ string "blue")

  blocksByColor :: Color -> [ColorBlocks] -> Int
  blocksByColor c cbs = fromMaybe 0 $ do
    ColorBlocks _ n <- L.find (\cb -> cb.color == c) cbs
    pure n

test :: IO ()
test = do
  -- inp <- readFile "app/Day2/input2.txt"
  b <- parseIO parseBlocks "1 blue, 2 green" "1 blue, 2 green"
  equals b (Blocks 0 2 1)

  g <- parseIO parseGame "inline" "Game 1: 3 blue, 4 red"
  equals g $ Game 1 [Blocks 4 0 3]

  gs <- parseIO parseGames "testInput" testInput
  mapM_ print gs

equals :: (Eq a, Show a) => a -> a -> IO ()
equals a b = do
  when (a /= b) $ do
    putStrLn "Expected (==)"
    putStrLn $ "\t" <> show a
    putStrLn $ "\t" <> show b
    fail "ASSERTION FAILED"

parseIO :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s a -> String -> s -> IO a
parseIO parser src inp =
  case parse parser src inp of
    Left bundle -> do
      putStrLn (errorBundlePretty bundle)
      fail "Failed Parse"
    Right g -> pure g

testInput :: String
testInput =
  [i|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|]
