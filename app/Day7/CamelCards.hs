module Day7.CamelCards where

import App.Parse
import App.Prelude
import Data.List (sort)
import Data.String.Interpolate (i)

test :: IO ()
test = do
  putStrLn "Camel Cards"
  trnd <- parseIO parseRound "testInput" testInput
  equals 765 (head trnd).bid
  equals (Hand C3 C2 T C3 K) (head trnd).hand
  equals (Hand Q Q Q J A) (last trnd).hand

  equals OnePair $ handTier $ Hand C3 C2 T C3 K
  equals FullHouse $ handTier $ Hand A Q Q A Q
  equals FiveOfAKind $ handTier $ Hand J J J J J
  equals FourOfAKind $ handTier $ Hand J J C3 J J
  equals TwoPair $ handTier $ Hand J J C3 Q C3
  equals ThreeOfAKind $ handTier $ Hand J J C3 J Q
  equals HighCard $ handTier $ Hand J K C3 A Q

  let hands = map (.hand) trnd
  [h0, h1, h2, h3, h4] <- pure hands
  equals (Hand Q Q Q J A) $ maximum hands
  equals (Hand C3 C2 T C3 K) $ minimum hands
  equals LT $ compare (Hand T T T C9 T) (Hand T T T T C9)

  equals [h0, h3, h2, h1, h4] $ sort hands

  print $ totalWinnings $ rankRound trnd

  putStrLn "Part 1"
  rnd <- parseFile "app/Day7/input7.txt" parseRound
  print $ totalWinnings $ rankRound rnd

  pure ()

testInput :: String
testInput =
  [i|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|]

totalWinnings :: [HandBidRank] -> Int
totalWinnings = sum . map winnings
 where
  winnings hbr = hbr.rank * hbr.bid

rankRound :: [HandBid] -> [HandBidRank]
rankRound hbs = zipWith make [1 ..] $ sortOn (.hand) hbs
 where
  make r hb = HandBidRank{rank = r, hand = hb.hand, bid = hb.bid}

handTier :: Hand -> Tier
handTier (Hand c1 c2 c3 c4 c5)
  | any (isLength 5) cards = FiveOfAKind
  | any (isLength 4) cards = FourOfAKind
  | any (isLength 3) cards && any (isLength 2) cards = FullHouse
  | any (isLength 3) cards = ThreeOfAKind
  | length (filter (isLength 2) cards) == 2 = TwoPair
  | any (isLength 2) cards = OnePair
  | otherwise = HighCard
 where
  cards = group $ sort [c1, c2, c3, c4, c5]

  isLength n cds = length cds == n

-- Types --------------------------------------------------------

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | J | Q | K | A
  deriving (Show, Eq, Ord)

data Hand = Hand Card Card Card Card Card
  deriving (Eq, Show)

instance Ord Hand where
  compare h1 h2 =
    case compare (handTier h1) (handTier h2) of
      EQ -> compareHighCards
      c -> c
   where
    cards :: Hand -> [Card]
    cards (Hand c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]

    compareHighCards :: Ordering
    compareHighCards =
      case filter (/= EQ) $ zipWith compare (cards h1) (cards h2) of
        [] -> EQ
        (a : _) -> a

data Tier
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

data HandBid = HandBid
  { hand :: Hand
  , bid :: Int
  }

data HandBidRank = HandBidRank
  { hand :: Hand
  , bid :: Int
  , rank :: Int
  }

parseRound :: Parser [HandBid]
parseRound = do
  parseHandBid `sepEndBy` newline

parseHandBid :: Parser HandBid
parseHandBid = do
  h <- parseHand
  space
  b <- decimal
  pure $ HandBid h b

parseHand :: Parser Hand
parseHand =
  Hand <$> parseCard <*> parseCard <*> parseCard <*> parseCard <*> parseCard

parseCard :: Parser Card
parseCard = do
  c <- anySingle
  card c
 where
  card 'A' = pure A
  card 'K' = pure K
  card 'Q' = pure Q
  card 'J' = pure J
  card 'T' = pure T
  card '9' = pure C9
  card '8' = pure C8
  card '7' = pure C7
  card '6' = pure C6
  card '5' = pure C5
  card '4' = pure C4
  card '3' = pure C3
  card '2' = pure C2
  card c = fail $ "card " <> show c
