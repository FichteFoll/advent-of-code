{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.List (sortOn, intercalate, transpose)
import Data.List.Split (splitOn)
import Data.Ord (Down(..))
import qualified Data.Map as Map

type Input = [String]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = lines

part1 :: Input -> Int
part1 = loadNorth . tiltNorth

part2 :: Input -> Int
part2 = loadNorth . untilRepeats 1000000000 . tail . iterate tiltAll

tiltNorth :: Input -> Input
tiltNorth = transpose . map (intercalate "#" . map (sortOn Down) . splitOn "#") . transpose

loadNorth :: Input -> Int
loadNorth
  = sum
  . zipWith (*) [1..]
  . map (length . filter (== 'O'))
  . reverse

untilRepeats :: Int -> [Input] -> Input
untilRepeats reps = go Map.empty 1
  where
    go _ _ [x] = x -- as if this is gonna happen
    go cache i axs@(x:xs) = case Map.lookup x cache of
      Just prevI -> let rest = (reps - prevI) `mod` (i - prevI) in
        axs !! rest
      Nothing    -> go (Map.insert x i cache) (succ i) xs
    go _ _ _ = undefined

tiltAll :: Input -> Input
tiltAll = (!! 4) . iterate (rotateRight . tiltNorth)

rotateRight :: Input -> Input
rotateRight = transpose . reverse
