{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main, parse, part1, part2) where

type Input = [Int]


main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (liftA2 ($) (op . head) (read . tail)) . lines
  where
    op 'L' = negate
    op 'R' = id

part1 :: Input -> Int
part1 = count . scanl (+) 50

part2 :: Input -> Int
part2 = count . concat . scanl (\a n -> take (abs n) $ tail $ iterate (+ signum n) $ last a) [50]

count :: [Int] -> Int
count = length . filter ((== 0) . flip mod 100)
