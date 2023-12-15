module Day4.Scratchcards where

import App.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

test :: IO ()
test = do
  putStrLn "ScratchCards"

  -- parsing
  let ci = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  card <- parseIO parseCard ci ci
  equals card.winning [41, 48, 83, 86, 17]
  equals card.numbers [83, 86, 6, 31, 17, 9, 48, 53]

  -- points
  equals (Points 0) $ points $ NumWinning 0
  equals (Points 4) $ points $ NumWinning 3
  equals (Points 8) $ points $ NumWinning 4

  equals (Points 8) $ points $ countWinning card

  -- test input
  ti <- readFile "app/Day4/input4.test.txt"
  tcs <- parseIO parseCards "input4.test.txt" ti
  equals 6 (length tcs)

  let ps = map (points . countWinning) tcs
  equals 13 (sum ps)

  -- actual input
  let inputPath = "app/Day4/input4.txt"
  inp <- readFile inputPath
  cards <- parseIO parseCards inputPath inp
  equals 219 (length cards)

  let pnts = map (points . countWinning) cards
  putStrLn "Part One"
  print $ sum pnts

  -- part two
  let start n = drop (n - 1)

  -- all copies
  equals [[6]] $ map (map (.id)) $ allCopies $ start 6 tcs
  equals [[5], [6]] $ map (map (.id)) $ allCopies $ start 5 tcs
  equals [[4, 5], [5], [6]] $ map (map (.id)) $ allCopies $ start 4 tcs
  equals 30 $ totalScratchcards tcs

  print $ totalScratchcards cards

data Copies = Copies
  { source :: Card
  , copies :: Copies
  }

type Parser = Parsec Void String

data Card = Card
  { id :: Int
  , winning :: [Int]
  , numbers :: [Int]
  }

instance Show Card where
  show c = show c.id

parseCards :: Parser [Card]
parseCards = many parseCard

parseCard :: Parser Card
parseCard = do
  _ <- string "Card"
  space
  d <- decimal
  _ <- char ':'
  space
  ws <- decimal `sepEndBy` space
  _ <- string "| "
  space
  ns <- decimal `sepEndBy` space
  pure $ Card d ws ns

newtype NumWinning = NumWinning Int
  deriving (Eq, Show)
newtype Points = Points Int
  deriving newtype (Eq, Show, Num)

countWinning :: Card -> NumWinning
countWinning c =
  NumWinning $ length $ filter isWinner c.numbers
 where
  isWinner n = n `elem` c.winning

points :: NumWinning -> Points
points (NumWinning 0) = Points 0
points (NumWinning n) =
  let ex = fromIntegral (n - 1) :: Float
      base = 2 :: Float
   in Points $ round (base ** ex)

-- Part 2 ------------------------------------------

-- https://github.com/mnvr/advent-of-code-2023/blob/main/04.hs
totalScratchcards :: [Card] -> Int
totalScratchcards =
  length . mconcat . allCopies

allCopies :: [Card] -> [[Card]]
allCopies =
  foldr f []
 where
  -- 6: just itself, ok
  -- 5, [[6]]
  -- 4: [[5], [6]]
  -- it's also WAY easier to see what's happening here and debug
  f :: Card -> [[Card]] -> [[Card]]
  f card wins = w : wins
   where
    NumWinning m = countWinning card
    w = card : mconcat (take m wins)
