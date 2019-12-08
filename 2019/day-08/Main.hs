module Main where

import Data.List
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)

parse :: String -> [Char]
parse = head . lines

layerize :: (Int, Int) -> [Char] -> [[Char]]
layerize size [] = []
layerize size@(w,h) inp = (take len inp):(layerize size $ drop len inp)
  where len = w*h

part1 :: [Char] -> Int
part1 input = (*) <$> count '1' <*> count '2'
  $ head $ sortWith (count '0') $ layerize (25,6) input
  where
    count :: Char -> [Char] -> Int
    count c s = length $ elemIndices c s

delayer :: [[Char]] -> [Char]
delayer layers = map (fromJust . find ('2' /=)) $ transpose layers

part2 :: [Char] -> [Char]
part2 = delayer . layerize (25,6)

showImg :: Int -> [Char] -> [Char]
showImg w [] = []
showImg w img = (map char $ take w img) ++ "\n" ++ (showImg w $ drop w img)
  where
    char '0' = ' '
    char '1' = '█'

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2:\n" ++ (showImg 25 $ part2 input)
