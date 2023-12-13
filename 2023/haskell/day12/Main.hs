{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2,countCombinations) where

import Data.List.Split (splitWhen)

type Input = [(String, [Int])]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map parseLine . lines
  where parseLine = fmap (map read . splitWhen (== ',') . tail) . break (== ' ')

part1 :: Input -> Int
part1 = sum . map ((uncurry .) countCombinations Nothing)

part2 :: Input -> Int
part2 _ = 0

countCombinations :: Maybe Int -> String -> [Int] -> Int
-- carry, line, counts
countCombinations Nothing  []       []     = 1
countCombinations (Just 0) []       []     = 1
countCombinations _        []       _      = 0
countCombinations Nothing  ('.':cs) ns     = countCombinations Nothing cs ns
countCombinations (Just 0) ('.':cs) ns     = countCombinations Nothing cs ns
countCombinations _        ('.':_)  _      = 0
countCombinations Nothing  ('#':_)  []     = 0
countCombinations Nothing  ('#':cs) (n:ns) = countCombinations (Just $ pred n) cs ns
countCombinations Nothing  ('?':cs) (n:ns) = countCombinations (Just $ pred n) cs ns + countCombinations Nothing cs (n:ns)
countCombinations Nothing  ('?':cs) ns     = countCombinations Nothing cs ns
countCombinations (Just 0) ('#':_)  _      = 0
countCombinations (Just 0) ('?':cs) ns     = countCombinations Nothing cs ns
countCombinations (Just n) ('#':cs) ns     = countCombinations (Just $ pred n) cs ns
countCombinations (Just n) ('?':cs) ns     = countCombinations (Just $ pred n) cs ns
countCombinations carry cs ns
  = error $ "not covered: " ++ show carry ++ " " ++ cs ++ " " ++ show ns

