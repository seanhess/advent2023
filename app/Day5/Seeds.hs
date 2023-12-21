module Day5.Seeds where

import App.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

-- Needs to be compiled with -O2!!!
-- `cabal build -O2`
test :: IO ()
test = do
  putStrLn "Seeds"
  let m1 = Mapping 50 98 2
  equals Nothing $ lookupMap 97 m1
  equals (Just 50) $ lookupMap 98 m1
  equals (Just 51) $ lookupMap 99 m1
  equals Nothing $ lookupMap 100 m1

  let m2 = Mapping 52 50 48
  let mts = [m1, m2]
  equals 0 $ lookupMaps 0 mts
  equals 1 $ lookupMaps 1 mts
  equals 48 $ lookupMaps 48 mts
  equals 49 $ lookupMaps 49 mts
  equals 52 $ lookupMaps 50 mts
  equals 53 $ lookupMaps 51 mts
  equals 81 $ lookupMaps 79 mts
  equals 14 $ lookupMaps 14 mts
  equals 57 $ lookupMaps 55 mts
  equals 13 $ lookupMaps 13 mts
  equals 98 $ lookupMaps 96 mts
  equals 51 $ lookupMaps 99 mts

  putStrLn "parsing"
  testInp <- readFile "app/Day5/input5.test.txt"
  testAl <- parseIO parseAlmanac "input5.test.txt" testInp

  equals 82 $ lookupLocation testAl 79
  equals 43 $ lookupLocation testAl 14
  equals 86 $ lookupLocation testAl 55
  equals 35 $ lookupLocation testAl 13

  equals 35 $ minimum $ allLocations testAl testAl.seeds

  putStrLn "Part 1"
  inp <- readFile "app/Day5/input5.txt"
  al <- parseIO parseAlmanac "input5.txt" inp

  print $ minimum $ allLocations al al.seeds

  putStrLn "Part 2"
  equals [(1, 2), (3, 4)] $ listPairs ([1, 2, 3, 4] :: [Int])
  -- equals [100, 101, 102] $ parseSeedsFromRange [100, 3]
  -- equals [100, 101, 102] $ parseSeedsFromRange [100, 3]
  -- equals [10, 11, 12, 21, 22] $ parseSeedsFromRange [10, 3, 21, 2]

  equals 46 $ lookupLocation testAl 82
  -- equals 46 $ minimum $ allLocations testAl (parseSeedsFromRange testAl.seeds)

  equals 50 $ lookupMapsReverse 52 mts
  equals 51 $ lookupMapsReverse 53 mts
  equals 79 $ lookupMapsReverse 81 mts
  equals 96 $ lookupMapsReverse 98 mts
  equals 99 $ lookupMapsReverse 51 mts

  equals 13 $ seedForLocation testAl 35
  -- equals 35 $ minimumLocation testAl testAl.seeds

  equals 46 $ minimumLocation testAl

  -- oh, it's a memoization problem
  -- let seeds = parseSeedsFromRange al.seeds
  putStrLn "Running..."
  print $ minimumLocation al

  putStrLn "done"

type Parser = Parsec Void String

newtype Id a = Id {value :: Int}
  deriving newtype (Show, Eq, Num, Ord, Enum)

data Seed
data Soil
data Fertilizer
data Water
data Light
data Temperature
data Humidity
data Location

data Mapping a b = Mapping
  { dest :: Id b
  , source :: Id a
  , range :: Int
  }

data Almanac = Almanac
  { seeds :: [Id Seed]
  , seedToSoil :: [Mapping Seed Soil]
  , soilToFert :: [Mapping Soil Fertilizer]
  , fertToWater :: [Mapping Fertilizer Water]
  , waterToLight :: [Mapping Water Light]
  , lightToTemp :: [Mapping Light Temperature]
  , tempToHumid :: [Mapping Temperature Humidity]
  , humidToLocation :: [Mapping Humidity Location]
  }

data ReverseAlmanac = ReverseAlmanac
  { seedToSoil :: [Mapping Soil Seed]
  , soilToFert :: [Mapping Fertilizer Soil]
  , fertToWater :: [Mapping Water Fertilizer]
  , waterToLight :: [Mapping Light Water]
  , lightToTemp :: [Mapping Temperature Light]
  , tempToHumid :: [Mapping Humidity Temperature]
  , humidToLocation :: [Mapping Location Humidity]
  }

parseAlmanac :: Parser Almanac
parseAlmanac = do
  seeds <- parseSeeds
  space

  seedToSoil <- parseMappings "seed-to-soil"
  space

  soilToFert <- parseMappings "soil-to-fertilizer"
  space

  fertToWater <- parseMappings "fertilizer-to-water"
  space

  waterToLight <- parseMappings "water-to-light"
  space

  lightToTemp <- parseMappings "light-to-temperature"
  space

  tempToHumid <- parseMappings "temperature-to-humidity"
  space

  humidToLocation <- parseMappings "humidity-to-location"

  pure $ Almanac{seeds, seedToSoil, soilToFert, fertToWater, waterToLight, lightToTemp, tempToHumid, humidToLocation}

parseSeeds :: Parser [Id Seed]
parseSeeds = do
  _ <- string "seeds: "
  decimal `sepEndBy` space

parseMappings :: String -> Parser [Mapping a b]
parseMappings name = do
  _ <- string name
  _ <- string " map:"
  _ <- newline
  parseMapping `sepEndBy` newline

parseMapping :: Parser (Mapping a b)
parseMapping = do
  dest <- decimal
  space
  source <- decimal
  space
  range <- decimal
  pure $ Mapping{source, dest, range}

lookupMaps :: Id a -> [Mapping a b] -> Id b
lookupMaps a ms =
  fromMaybe (Id a.value)
    $ listToMaybe
    $ mapMaybe (lookupMap a) ms

lookupMap :: forall a b. Id a -> Mapping a b -> Maybe (Id b)
lookupMap ia m = do
  let mn = m.source.value
      mx = mn + m.range :: Int
      a = ia.value
  guard (mn <= a && a < mx)
  pure $ Id $ m.dest.value + (a - m.source.value)

lookupLocation :: Almanac -> Id Seed -> Id Location
lookupLocation al a =
  let soil = lookupMaps a al.seedToSoil
      fert = lookupMaps soil al.soilToFert
      watr = lookupMaps fert al.fertToWater
      lght = lookupMaps watr al.waterToLight
      temp = lookupMaps lght al.lightToTemp
      humd = lookupMaps temp al.tempToHumid
      loct = lookupMaps humd al.humidToLocation
   in loct

allLocations :: Almanac -> [Id Seed] -> [Id Location]
allLocations al = map (lookupLocation al)

lookupMapReverse :: forall a b. Id b -> Mapping a b -> Maybe (Id a)
lookupMapReverse ib m = do
  let mn = m.dest.value
      mx = mn + m.range :: Int
      b = ib.value
  guard (mn <= b && b < mx)
  pure $ Id $ m.source.value + (b - m.dest.value)

lookupMapsReverse :: Id b -> [Mapping a b] -> Id a
lookupMapsReverse b ms =
  fromMaybe (Id b.value)
    $ listToMaybe
    $ mapMaybe (lookupMapReverse b) ms

seedForLocation :: Almanac -> Id Location -> Id Seed
seedForLocation al loc =
  let humd = lookupMapsReverse loc al.humidToLocation
      temp = lookupMapsReverse humd al.tempToHumid
      lght = lookupMapsReverse temp al.lightToTemp
      watr = lookupMapsReverse lght al.waterToLight
      fert = lookupMapsReverse watr al.fertToWater
      soil = lookupMapsReverse fert al.soilToFert
   in lookupMapsReverse soil al.seedToSoil

listPairs :: [a] -> [(a, a)]
listPairs [] = []
listPairs [_] = []
listPairs (a : b : rest) = (a, b) : listPairs rest

seedRanges :: [Id Seed] -> [(Id Seed, Int)]
seedRanges seeds =
  map (\(s, r) -> (s, r.value)) $ listPairs seeds

isSeed :: [(Id Seed, Int)] -> Id Seed -> Bool
isSeed rngs seed = any isRange rngs
 where
  isRange (s, r) = s.value <= seed.value && seed.value < s.value + r

minimumLocation :: Almanac -> Id Location
minimumLocation al =
  let rngs = seedRanges al.seeds
      locs = [0 ..] :: [Id Location]
      sds = map (seedForLocation al) locs :: [Id Seed]
   in snd $ head $ filter (\(s, _) -> isSeed rngs s) $ zip sds locs
