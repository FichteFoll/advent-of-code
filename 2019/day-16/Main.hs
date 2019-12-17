module Main where

import Data.List.Split (chunksOf)

parse :: String -> [Int]
parse = map read . chunksOf 1 . (head . lines)

fft :: [Int] -> [Int]
fft list = map (abs . flip rem 10 . sum) $ zipWith (zipWith (*)) (replicate (length list) list) patterns
  where
    patterns = map (drop 1) [concatMap (replicate i) $ cycle [0,1,0,-1] | i <- [1..]]

part1 :: [Int] -> String
part1 = concatMap show . take 8 . (!! 100) . iterate fft

part2 :: [Int] -> String
part2 input
  | offset < (10000 * length input `div` 2) = error "Input can't be solved"
  | otherwise = concatMap show $ take 8 $ iterate fft2 realInput !! 100
  where
    offset = read $ concatMap show $ take 7 input
    realInput = drop offset $ concat $ replicate 10000 input
    fft2 :: [Int] -> [Int]
    fft2 = scanr1 ((flip rem 10 .) . (+))

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ part1 input
  putStrLn $ "Part 2: " ++ part2 input
