module Main where

import Control.Applicative (liftA2)
import Data.Char

type Input = [String]

parse :: String -> Input
parse = lines

part1 :: Input -> Int
part1 = sum . map (liftA2 (+) ((* 10) . head) last . map digitToInt . filter isDigit)

part2 :: Input -> Int
part2 xs = 0

main :: IO ()
main = do
    input <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)
