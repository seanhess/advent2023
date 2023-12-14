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
  equals [2, 3, 4, 5] $ map (.id) $ copies tcs
  equals [3, 4] $ map (.id) $ copies $ drop 1 tcs

  print $ map (.id) tcs
  print $ allCards tcs

  print $ mconcat $ allCards tcs

-- zipWithM_ check cards pnts
-- check c n = do
--   putStrLn $ (show n) <> "\t" <> show c
-- print $ sum pnts

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

allCards :: [Card] -> [[Card]]
allCards [] = []
allCards (c : cx) =
  let cps = copies (c : cx)
   in [c : cps] <> allCards cx <> allCards cps

--   let NumWinning win = countWinning cd
--       msg = show ("ID", cd.id, map (.id) $ take win cds)
--    in trace msg $ 1 + processCards cds + processCards (take win cds)

copies :: [Card] -> [Card]
copies [] = []
copies (cd : cds) =
  let NumWinning win = countWinning cd
   in take win cds
