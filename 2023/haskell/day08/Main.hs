{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main, parse, part1, part2) where

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Data.Map (Map, (!), (!?))
import Control.Exception (assert)

type Network = Map String (String, String)
type Input = (String, Network)
-- map current pos & instruction pointer to total step count
type LoopCache = Map (String, Int) Int

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = (head &&& Map.fromList . map parseMapLine . drop 2) . lines
  where
    parseMapLine = take 3 &&& take 3 . drop 7 &&& take 3 . drop 12

part1 :: Input -> Int
part1 (lrs, net) = countSteps net "AAA" (cycle lrs)

part2 :: Input -> Int
part2 input@(_, net) = foldl1 lcm loops
  where
    loops = [findLoop input Map.empty pos 0 | pos <- Map.keys net, last pos == 'A']

countSteps :: Network -> String -> String -> Int
countSteps _ "ZZZ" _ = 0
countSteps net pos (lr:lrs) = 1 + countSteps net (nextPos net lr pos) lrs

nextPos :: Network -> Char -> String -> String
nextPos net lr pos = getter lr $ net ! pos
  where
    getter 'L' = fst
    getter 'R' = snd

findLoop :: Input -> LoopCache -> String -> Int -> Int
findLoop inp@(lrs, net) cache pos step
  = case cache !? key of
      -- At least for my input,
      -- all loops have the same length as their offset of first appearance,
      -- which makes calculating the lowest common multiple much easier.
      -- Additionally, all paths lead to a single node ending with 'Z' at the end of a whole cycle,
      -- which cannot be said about the example input.
      Just prevStep -> assert (prevStep * 2 == step) prevStep
      Nothing -> findLoop inp cache' pos' (succ step)
    where
      key = (pos, stepPointer)
      stepPointer = step `mod` length lrs
      pos' = nextPos net (lrs !! stepPointer) pos
      cache'
        | last pos == 'Z' = Map.insert key step cache
        | otherwise = cache
