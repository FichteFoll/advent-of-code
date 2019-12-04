module Main where

import Data.List.Split (splitOn)
import qualified Data.List as List
import qualified Data.Set as Set


parse :: String -> [Int]
parse str = [a..b]
  where (a:b:_) = map read $ splitOn "-" str

isPass :: Int -> Bool
isPass n = all id [(List.sort digits) == digits
                  , (length $ Set.fromList digits) < length digits]
  where digits = show n

isPass2 :: Int -> Bool
isPass2 n = all id [(List.sort digits) == digits
                  , (length $ Set.fromList digits) < length digits
                  , any (== 2) $ map (\d -> length $ List.elemIndices d digits) ['1'..'9']]
  where digits = show n

part1 :: [Int] -> Int
part1 input = length [True | n <- [head input..last input], isPass n]

part2 :: [Int] -> Int
part2 input = length [True | n <- [head input..last input], isPass2 n]


main :: IO ()
main = do
  let input = parse "264793-803935"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
