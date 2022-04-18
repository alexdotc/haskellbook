{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

dec1 = "5.67"
dec2 = "-1.5"
bad = "1.-5"

parseFraction :: (Monad m, MonadFail m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- Exercise Try Try pg 943

parseDecimal :: Parser Double
parseDecimal = do
  n <- many (char '-')
  d <- double
  if null n then return d else return ((-1)*d)

parseDecimalOrFraction :: Parser (Either Rational Double)
parseDecimalOrFraction = try (Left <$> parseFraction) <|> (Right <$> parseDecimal)

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  let parseDecimal' =
        parseString parseDecimal mempty
  let parseDF' =
        parseString parseDecimalOrFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseDecimal' dec1
  print $ parseDecimal' dec2
  print $ parseDF' dec1
  print $ parseDF' shouldWork
  print $ parseDecimal' bad

main2 :: IO ()
main2 = do
  let attoP = parseOnly parseFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad
  let p f i =
        parseString f mempty i
  print $ p parseFraction badFraction
  print $ p parseFraction shouldWork
  print $ p parseFraction shouldAlsoWork
  print $ p parseFraction alsoBad
