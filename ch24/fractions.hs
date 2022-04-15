{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

dec1 = "5.67"
dec2 = "-1.5"
bad = "1.-5"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
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
parseDecimalOrFraction = try (Left <$> virtuousFraction) <|> (Right <$> parseDecimal)

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

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' badFraction
