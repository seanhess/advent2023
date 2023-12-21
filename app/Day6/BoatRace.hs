module Day6.BoatRace where

import App.Prelude
import Data.Char (isDigit)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

test :: IO ()
test = do
  putStrLn "Boat Race"
  putStrLn "Test"
  trrs <- parseIO parseRaceRecords "testInput" testInput
  let r1 = head trrs
  equals (RaceRecord 7 9) (head trrs)
  equals (RaceRecord 30 200) (last trrs)

  equals 0 $ raceDistance r1 0
  equals 6 $ raceDistance r1 1
  equals 10 $ raceDistance r1 2
  equals 12 $ raceDistance r1 3
  equals 12 $ raceDistance r1 4
  equals 10 $ raceDistance r1 5
  equals 6 $ raceDistance r1 6
  equals 0 $ raceDistance r1 7

  equals [1, 2, 3, 4, 5, 6] $ holdTimes r1
  equals [2, 3, 4, 5] $ goodTimes r1
  equals 288 $ marginOfError trrs

  putStrLn "Part 1"
  inp <- readFile "app/Day6/input6.txt"
  rrs <- parseIO parseRaceRecords "input6.txt" inp

  print $ marginOfError rrs

  putStrLn "Part 2"
  trr2 <- parseIO parseRaceRecord2 "testInput" testInput
  equals (RaceRecord 71530 940200) trr2
  equals 71503 $ length (goodTimes trr2)

  rr2 <- parseIO parseRaceRecord2 "input6.txt" inp
  print $ length (goodTimes rr2)

-- Basic Race ---------------------------------

marginOfError :: [RaceRecord] -> Int
marginOfError rrs = product $ map (length . goodTimes) rrs

goodTimes :: RaceRecord -> [Time]
goodTimes rr = filter (isWinner rr) $ holdTimes rr

isWinner :: RaceRecord -> Time -> Bool
isWinner rr t = raceDistance rr t > rr.distance

holdTimes :: RaceRecord -> [Time]
holdTimes rr = [1 .. rr.time - 1]

raceDistance :: RaceRecord -> Time -> Distance
raceDistance rr hold =
  distance (rr.time - hold) (speed hold)

speed :: Time -> Speed
speed (Time a) = Speed a

distance :: Time -> Speed -> Distance
distance (Time t) (Speed s) = Distance (t * s)

-- Types --------------------------------------
newtype Time = Time {value :: Int}
  deriving newtype (Num, Eq, Show, Enum)

newtype Distance = Distance {value :: Int}
  deriving newtype (Num, Eq, Show, Ord)

newtype Speed = Speed {value :: Int}
  deriving newtype (Num, Eq, Show)

data RaceRecord = RaceRecord
  { time :: Time
  , distance :: Distance
  }
  deriving (Eq, Show)

--- Parsing ---------------------------------
type Parser = Parsec Void String

parseRaceRecords :: Parser [RaceRecord]
parseRaceRecords = do
  _ <- string "Time:"
  space
  ts <- decimal `sepEndBy` space
  _ <- string "Distance:"
  space
  ds <- decimal `sepEndBy` space
  pure $ zipWith RaceRecord ts ds

parseRaceRecord2 :: Parser RaceRecord
parseRaceRecord2 = do
  _ <- string "Time:"
  space
  t <- num
  space
  _ <- string "Distance:"
  space
  d <- num
  pure $ RaceRecord (Time t) (Distance d)
 where
  num :: Parser Int
  num = do
    ncs <- many (spaceChar <|> digitChar)
    pure $ read $ filter isDigit ncs

testInput :: String
testInput = "Time:      7  15   30\nDistance:  9  40  200"
