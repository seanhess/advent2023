module Day3.EngineParts where

import App.Prelude
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- Parsing -------------------------------------------------
data Loc = Loc
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq)

data IndexedChar = IndexedChar
  { loc :: Loc
  , char :: Char
  }
  deriving (Show, Eq)

data Grid = Grid
  { chars :: [[Char]]
  , maxX :: Int
  , maxY :: Int
  }

grid :: [String] -> Grid
grid [] = Grid [] 0 0
grid chars@(r : _) =
  let maxX = length r - 1
      maxY = length chars - 1
   in Grid{chars, maxX, maxY}

parseCharsIndexed :: Grid -> [IndexedChar]
parseCharsIndexed g =
  mconcat . indexRows $ g.chars
 where
  indexRows :: [String] -> [[IndexedChar]]
  indexRows rows =
    let irs = map index rows :: [[(Int, Char)]]
     in zipWith indexRow [0 ..] irs

  indexRow :: Int -> [(Int, Char)] -> [IndexedChar]
  indexRow y = map indexedChar
   where
    indexedChar (x, char) = IndexedChar{loc = Loc{x, y}, char}

  index :: [Char] -> [(Int, Char)]
  index = zip [0 ..]

-- Surrounding Chars  ---------------------------------------
surrounding :: Int -> Int -> Loc -> [Loc]
surrounding mx my Loc{x, y} =
  let xs = [x - 1, x, x + 1]
      ys = [y - 1, y, y + 1]
   in [Loc x' y' | x' <- xs, y' <- ys, notOrigin x' y', notTooBig x' y', notNegative x' y']
 where
  notOrigin x' y' = not (x' == x && y' == y)
  notNegative x' y' = x' >= 0 && y' >= 0
  notTooBig x' y' = x' <= mx && y' <= my

findChars :: Grid -> [Loc] -> [Char]
findChars g =
  map findChar
 where
  findChar :: Loc -> Char
  findChar loc =
    let row = g.chars !! loc.y
     in row !! loc.x

-- Grouping ----------------------------------------------------

newtype PartNumber = PartNumber Int
  deriving newtype (Num, Show, Eq)

partNumbers :: Grid -> [IndexedChar] -> [PartNumber]
partNumbers g ics =
  mapMaybe parseGroup $ groupByDigits ics
 where
  parseGroup :: [IndexedChar] -> Maybe PartNumber
  parseGroup grp = do
    guard (any isNearSymbol grp)
    n <- readGroup grp
    pure $ PartNumber n

  isNearSymbol :: IndexedChar -> Bool
  isNearSymbol ic =
    any isSymbol $ findChars g (surrounding g.maxX g.maxY ic.loc)

  isSymbol :: Char -> Bool
  isSymbol '.' = False
  isSymbol c = not (isDigit c)

readGroup :: [IndexedChar] -> Maybe Int
readGroup ics =
  readMaybe $ map (.char) ics

groupByDigits :: [IndexedChar] -> [[IndexedChar]]
groupByDigits = groupBy byDigits
 where
  byDigits :: IndexedChar -> IndexedChar -> Bool
  byDigits c1 c2 = isDigit c1.char && isDigit c2.char

parsePartNumbers :: [String] -> [PartNumber]
parsePartNumbers lns =
  let g = grid lns
      ics = parseCharsIndexed g
   in partNumbers g ics

test :: IO ()
test = do
  putStrLn "Engine Parts"
  ti <- readFile "app/Day3/test_input3.txt"

  -- Parsing
  let g = grid $ lines ti
  let ics = parseCharsIndexed g
  let plus = head $ filter (\ic -> ic.char == '+') ics
  plus.loc `equals` Loc{x = 5, y = 5}

  -- Surrounding
  surrounding 0 0 (Loc 0 0) `equals` []
  surrounding 1 0 (Loc 0 0) `equals` [Loc 1 0]
  surrounding 1 1 (Loc 0 0) `equals` [Loc 0 1, Loc 1 0, Loc 1 1]
  surrounding 2 2 (Loc 1 1) `equals` [Loc 0 0, Loc 0 1, Loc 0 2, Loc 1 0, Loc 1 2, Loc 2 0, Loc 2 1, Loc 2 2]

  -- Surrounding Chars
  let dolla = head $ filter (\ic -> ic.char == '$') ics
  let around = findChars g (surrounding 10 10 dolla.loc)
  let nums = filter isDigit around
  nums `equals` "64"

  -- Part Numbers
  let pnums = partNumbers g ics
  sum pnums `equals` 4361

  -- Final Answer
  inp <- readFile "app/Day3/input3.txt"
  let ps = parsePartNumbers (lines inp)
  print $ sum ps
