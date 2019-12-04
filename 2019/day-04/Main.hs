module Main where

import Data.List.Split (splitOn)
import Data.List


parse :: String -> [Int]
parse str = [a..b]
  where (a:b:_) = map read $ splitOn "-" str

isPass :: [Char] -> Bool
isPass digits = and [(sort digits) == digits
                    , (length $ nub digits) /= length digits]

isPass2 :: [Char] -> Bool
isPass2 digits = and [(sort digits) == digits
                     , elem 2 $ map length $ group digits]

numMatches :: ([Char] -> Bool) -> [Int] -> Int
numMatches test input = length [True | n <- [head input..last input], test $ show n]

part1 :: [Int] -> Int
part1 = numMatches isPass

part2 :: [Int] -> Int
part2 = numMatches isPass2


main :: IO ()
main = do
  let input = parse "264793-803935"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
