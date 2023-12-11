{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

type Input = [[Int]]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (map read . words) . lines

part1 :: Input -> Int
part1 = sum . map nextNum

part2 :: Input -> Int
part2 _ = 0

nextNum :: [Int] -> Int
nextNum ns
  | all (== 0) ns = 0
  | otherwise = last ns + nextNum (derive ns)

derive :: [Int] -> [Int]
derive [a, b] = [b - a]
derive (a:ns@(b:_)) = b - a : derive ns
derive ns = error $ "list too short " ++ show ns
