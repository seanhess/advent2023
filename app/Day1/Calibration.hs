module Day1.Calibration where

import App.Prelude
import Data.Char (isDigit)
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Calibration = Calibration {num :: Int}
  deriving newtype (Num, Read)

newtype Digit = Digit {char :: Int}
  deriving newtype (Num)

sumCalibrations :: [String] -> Calibration
sumCalibrations =
  sum . map (calibration . digits)

calibration :: [Digit] -> Calibration
calibration [] = Calibration 0
calibration ds =
  let Digit d1 = head ds
      Digit d2 = last ds
   in read $ show d1 <> show d2

digits :: String -> [Digit]
digits = map (Digit . read . (: [])) . filter isDigit

toDigit :: Char -> Digit
toDigit c = Digit $ read [c]

----- Part Two ---------------------------------------
-- We need a real parser library! Enter Megaparsec

type Parser = Parsec String String

data Tok
  = Dig Digit
  | Chr Char

tokenDigit :: Tok -> Maybe Digit
tokenDigit (Dig d) = Just d
tokenDigit _ = Nothing

digitsTrue :: String -> [Digit]
digitsTrue l =
  case runParser parseTokens l l of
    Left _ -> []
    Right ds -> mapMaybe tokenDigit ds

parseTokens :: Parser [Tok]
parseTokens = many parseToken

parseToken :: Parser Tok
parseToken = do
  Dig <$> parseDigit <|> Chr <$> asciiChar

parseDigit :: Parser Digit
parseDigit = parseWords <|> toDigit <$> digitChar
 where
  parseWords :: Parser Digit
  parseWords =
    word "one"
      <|> word "two"
      <|> word "three"
      <|> word "four"
      <|> word "five"
      <|> word "six"
      <|> word "seven"
      <|> word "eight"
      <|> word "nine"
      <|> word "ten"

  word s = digitWord <$> string s

  digitWord :: String -> Digit
  digitWord "one" = 1
  digitWord "two" = 2
  digitWord "three" = 3
  digitWord "four" = 4
  digitWord "five" = 5
  digitWord "six" = 6
  digitWord "seven" = 7
  digitWord "eight" = 8
  digitWord "nine" = 9
  digitWord "zero" = 0
  digitWord _ = 0

sumCalibrations2 :: [String] -> Calibration
sumCalibrations2 =
  sum . map (calibration . digitsTrue)
