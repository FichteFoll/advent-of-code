module Main (main, parse, part1, part2) where

import Control.Applicative
import Data.Char
import Debug.Trace

type Cube = (String, Int)
type Input = [[[Cube]]]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
parse :: String -> Input
parse = map (sets . content) . lines
  where
    content = drop 2 . dropWhile (/= ':')
    sets = map cubes . wordsWhen (== ';')
    cubes = map parseCube . wordsWhen (== ',')
    parseCube x = case words x of
      [n, color] -> (color, read n)
      _ -> error $ "unable to parse cube text: " ++ x

part1 :: Input -> Int
part1 = sum . map fst . filter isAllowed . zip [1..]
  where
    allowed :: [Cube]
    allowed = [("red", 12), ("green", 13), ("blue", 14)]
    isAllowed (_, sets) = null
      [ True
      | cubes <- sets
      , (col1, n1) <- cubes
      , (col2, n2) <- allowed
      , col1 == col2
      , n1 > n2
      ]

part2 :: Input -> Int
part2 x = 0


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s
  = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'
