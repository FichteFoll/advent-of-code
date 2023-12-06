module Main (main, parse, part1, part2, extractDigits2, calcLine) where

import Control.Applicative (liftA2)
import Data.Char (digitToInt, isDigit)

type Input = [String]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = lines

part1 :: Input -> Int
part1 = sum . map (calcLine . extractDigits1)

part2 :: Input -> Int
part2 = sum . map (calcLine . extractDigits2)

extractDigits1 :: String -> [Int]
extractDigits1 = map digitToInt . filter isDigit

extractDigits2 :: String -> [Int]
-- keep the last character to support overlapping words (needs at most one char)
extractDigits2 ('o':'n':xs@('e':_))         = 1:extractDigits2 xs
extractDigits2 ('t':'w':xs@('o':_))         = 2:extractDigits2 xs
extractDigits2 ('t':'h':'r':'e':xs@('e':_)) = 3:extractDigits2 xs
extractDigits2 ('f':'o':'u':xs@('r':_))     = 4:extractDigits2 xs
extractDigits2 ('f':'i':'v':xs@('e':_))     = 5:extractDigits2 xs
extractDigits2 ('s':'i':xs@('x':_))         = 6:extractDigits2 xs
extractDigits2 ('s':'e':'v':'e':xs@('n':_)) = 7:extractDigits2 xs
extractDigits2 ('e':'i':'g':'h':xs@('t':_)) = 8:extractDigits2 xs
extractDigits2 ('n':'i':'n':xs@('e':_))     = 9:extractDigits2 xs
extractDigits2 (c:xs) | isDigit c           = digitToInt c:extractDigits2 xs
extractDigits2 (_:xs)                       = extractDigits2 xs
extractDigits2 []                           = []

calcLine :: [Int] -> Int
calcLine = liftA2 (+) ((* 10) . head) last
