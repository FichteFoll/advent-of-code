{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.Map.Strict as M
import Data.List.Extra (splitOn)
import Control.Lens
import Linear.V4

type Input = M.Map String [String]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = M.fromList . map ((\[k, v] -> (k, words v)) . splitOn ": ") . lines

part1 :: Input -> Int
part1 = sum . walk "you"

part2 :: Input -> Int
part2 = view _w . walk "svr"

walk :: String -> Input -> V4 Int
walk start graph = walk' (M.singleton "out" $ V4 1 0 0 0) start M.! start
  where
    walk' :: M.Map String (V4 Int) -> String -> M.Map String (V4 Int)
    walk' cache pos
      | pos `M.member` cache = cache
      | otherwise = M.insert pos (merge pos $ sum $ map (cache' M.!) leaves) cache'
      where
        cache' = foldl walk' cache leaves
        leaves = graph M.! pos
    merge pos state@(V4 none dac fft both)
      | pos == "fft" = V4 0 0 none (dac + both)
      | pos == "dac" = V4 0 none 0 (fft + both)
      | otherwise = state
