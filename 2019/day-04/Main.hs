module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set


parse :: String -> [Int]
parse str = [a..b]
  where (a:b:_) = map read $ splitOn "-" str

isPass :: Int -> Bool
isPass n = all id [(List.sort digits) == digits
                  , (length $ Set.fromList digits) < length digits]
  where digits = show n

part1 :: [Int] -> Int
part1 input = length [True | n <- [head input..last input], isPass n]


main :: IO ()
main = do
  let input = parse "264793-803935"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  -- putStrLn $ "Part 2: " ++ (show $ part2 input)
