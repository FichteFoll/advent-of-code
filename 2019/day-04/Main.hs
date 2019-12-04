module Main where

import Data.List.Split (splitOn)
import qualified Data.List as List
import qualified Data.Set as Set


parse :: String -> [Int]
parse str = [a..b]
  where (a:b:_) = map read $ splitOn "-" str

isPass :: [Char] -> Bool
isPass digits = and [(List.sort digits) == digits
                    , (length $ Set.fromList digits) < length digits]

isPass2 :: [Char] -> Bool
isPass2 digits = and [isPass digits
                     , any (== 2) $ map (\c -> length $ List.elemIndices c digits) ['1'..'9']]

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
