module Cis194.ValidateCardNumber where

import           Data.List

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev x
  | x < 1 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther list = reverse . advance Skipp $ (reverse  list)

data Position = Multiply | Skipp

advance :: Position -> [Integer]  -> [Integer]
advance _ [] = []
advance Skipp (x:xs) = x : advance Multiply xs
advance Multiply (x:xs)  = 2 * x : advance Skipp xs

splitIntoDigits :: [Integer] -> [Integer]
splitIntoDigits = concatMap $ toDigits

sumDigits = sum . splitIntoDigits

validate :: Integer -> Bool
validate cardNumber = (mod . sumDigits . doubleEveryOther . toDigits $ cardNumber) 10 == 0

