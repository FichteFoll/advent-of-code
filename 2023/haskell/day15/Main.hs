{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2, hash) where
import Data.Char (ord)
import Data.List.Split (splitOn)

type Input = [String]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = splitOn "," . concat . lines -- strip trailing newline

part1 :: Input -> Int
part1 = sum . map hash

hash :: String -> Int
hash = foldl step 0
  where step acc c = ((acc + ord c) * 17) `mod` 256

part2 :: Input -> Int
part2 _ = 0
