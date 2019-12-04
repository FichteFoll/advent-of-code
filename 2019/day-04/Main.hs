module Main where

import Data.List.Split (splitOn)
import Data.List

isPass :: [Char] -> Bool
isPass digits = and [sort digits == digits
                    , length (nub digits) /= length digits]

isPass2 :: [Char] -> Bool
isPass2 digits = and [sort digits == digits
                     , elem 2 $ map length $ group digits]

numMatches :: ([Char] -> Bool) -> [Int] -> Int
numMatches test input = length $ filter (test . show) input

part1 :: [Int] -> Int
part1 = numMatches isPass

part2 :: [Int] -> Int
part2 = numMatches isPass2

main :: IO ()
main = do
  let input = [264793..803935]
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
