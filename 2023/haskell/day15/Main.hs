{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2, hash) where

import Control.Applicative (liftA2)
import Control.Lens ((&), (%~), at)
import Data.Char (ord, digitToInt)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type Input = [String]
type Boxes = Map Int [(String, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = splitOn "," . concat . lines -- strip trailing newline

part1 :: Input -> Int
part1 = sum . map hash

part2 :: Input -> Int
part2 = power . foldl insert Map.empty

hash :: String -> Int
hash = foldl step 0
  where step acc c = ((acc + ord c) * 17) `mod` 256

insert :: Boxes -> String -> Boxes
insert boxes next = boxes & at k %~ case cmd of
    '=' -> Just . insert' (label, flen) . fromMaybe []
    '-' -> fmap (filter $ (/= label) . fst)
    _ -> error $ next ++ " " ++ [cmd]
  where
    (label, cmd:num) = break (liftA2 (||) (== '-') (== '=')) next
    k = hash label
    flen = digitToInt $ head num -- num can be empty!
    insert' new [] = [new]
    insert' (l', n) ((l, _):xs) | l == l' = (l, n) : xs
    insert' new (x:xs) = x : insert' new xs

power :: Boxes -> Int
power = sum . concat . Map.mapWithKey power'
  where power' k = map (* succ k) . zipWith (*) [1..] . map snd
