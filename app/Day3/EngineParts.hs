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

data Grid a = Grid
  { rows :: [[a]]
  , maxX :: Int
  , maxY :: Int
  }

grid :: [[a]] -> Grid a
grid [] = Grid [] 0 0
grid rows@(r : _) =
  let maxX = length r - 1
      maxY = length rows - 1
   in Grid{rows, maxX, maxY}

parseCharsIndexed :: Grid Char -> Grid IndexedChar
parseCharsIndexed g =
  grid . indexRows $ g.rows
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

findChars :: Grid IndexedChar -> [Loc] -> [IndexedChar]
findChars g =
  map findChar
 where
  findChar :: Loc -> IndexedChar
  findChar loc =
    let row = g.rows !! loc.y
     in row !! loc.x

-- Grouping ----------------------------------------------------

data PartNumber = PartNumber
  { number :: Int
  , symbols :: [IndexedChar]
  }
  deriving (Show, Eq)

partNumbers :: Grid IndexedChar -> [PartNumber]
partNumbers g =
  mapMaybe parseGroup $ groupByDigits $ mconcat g.rows
 where
  parseGroup :: [IndexedChar] -> Maybe PartNumber
  parseGroup grp = do
    let syms = mconcat $ map nearbySymbols grp
    guard $ not (null syms)
    n <- readGroup grp
    pure $ PartNumber n syms

  nearbySymbols :: IndexedChar -> [IndexedChar]
  nearbySymbols ic =
    filter isSymbol $ findChars g (surrounding g.maxX g.maxY ic.loc)

  isSymbol :: IndexedChar -> Bool
  isSymbol ic = not (isDigit ic.char) && ic.char /= '.'

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
  partNumbers $ parseCharsIndexed $ grid lns

-- Part 2 --------------------------------------------------

data Gear = Gear PartNumber PartNumber
  deriving (Show)

isPartAdjacent :: Loc -> PartNumber -> Bool
isPartAdjacent Loc{x, y} part = any isLoc part.symbols
 where
  isLoc s = s.loc.x == x && s.loc.y == y

gridStars :: Grid IndexedChar -> [IndexedChar]
gridStars g = filter (\c -> c.char == '*') $ mconcat g.rows

starParts :: IndexedChar -> [PartNumber] -> [PartNumber]
starParts star = filter (isPartAdjacent star.loc)

gears :: [PartNumber] -> Grid IndexedChar -> [Gear]
gears parts =
  mapMaybe toGear . gridStars
 where
  toGear :: IndexedChar -> Maybe Gear
  toGear star = do
    case starParts star parts of
      [p1, p2] -> pure $ Gear p1 p2
      _ -> Nothing

gearRatio :: Gear -> Int
gearRatio (Gear (PartNumber a _) (PartNumber b _)) = a * b

test :: IO ()
test = do
  putStrLn "Engine Parts"
  ti <- readFile "app/Day3/test_input3.txt"

  -- Parsing
  let g = grid $ lines ti
  let gi = parseCharsIndexed g
  let plus = head $ filter (\ic -> ic.char == '+') $ mconcat gi.rows
  plus.loc `equals` Loc{x = 5, y = 5}

  -- Surrounding
  surrounding 0 0 (Loc 0 0) `equals` []
  surrounding 1 0 (Loc 0 0) `equals` [Loc 1 0]
  surrounding 1 1 (Loc 0 0) `equals` [Loc 0 1, Loc 1 0, Loc 1 1]
  surrounding 2 2 (Loc 1 1) `equals` [Loc 0 0, Loc 0 1, Loc 0 2, Loc 1 0, Loc 1 2, Loc 2 0, Loc 2 1, Loc 2 2]

  -- Surrounding Chars
  let dolla = head $ filter (\ic -> ic.char == '$') $ mconcat gi.rows
  let around = map (.char) $ findChars gi (surrounding 10 10 dolla.loc)
  let nums = filter isDigit around
  nums `equals` "64"

  -- Part Numbers
  let pnums = partNumbers gi
  sum (map (.number) pnums) `equals` 4361

  -- Gears
  let gs = gears pnums gi
  map gearRatio gs `equals` [16345, 451490]

  -- Part 1
  inp <- readFile "app/Day3/input3.txt"
  let ps = parsePartNumbers (lines inp)
  print $ sum (map (.number) ps)

  -- Part 2
  let gi2 = parseCharsIndexed $ grid $ lines inp
  let gs2 = gears ps gi2
  print $ sum $ map gearRatio gs2
