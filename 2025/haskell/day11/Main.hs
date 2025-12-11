{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.Map.Strict as M
import Data.List.Extra (splitOn)

type Input = M.Map String [String]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = M.fromList . map ((\[k, v] -> (k, words v)) . splitOn ": ") . lines

part1 :: Input -> Int
part1 graph = walk (M.singleton "out" 1) "you" M.! "you"
  where
    walk :: M.Map String Int -> String -> M.Map String Int
    walk cache pos
      | pos `M.member` cache = cache
      | otherwise = M.insert pos (sum $ map (cache' M.!) leaves) cache'
      where
        cache' = foldl walk cache leaves
        leaves = graph M.! pos

part2 :: Input -> Int
part2 _ = 0
