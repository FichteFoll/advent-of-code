module Main where

-- import Data.List
import Data.List.Split (chunksOf)

parse :: String -> [Int]
parse = map read . chunksOf 1 . (head . lines)

fft :: [Int] -> [Int]
fft list = map (getDigit . sum) $ zipWith (zipWith (*)) (replicate (length list) list) patterns
  where
    getDigit = abs . flip rem 10
    basePattern = [0,1,0,-1]
    patterns = map (drop 1) [concatMap (replicate i) $ cycle basePattern | i <- [1..]]

part1 :: [Int] -> String
part1 input = concatMap show $ take 8 $ iterate fft input !! 100

-- part2 :: [Int] -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ part1 input
  -- putStrLn $ "Part 2: " ++ show (part2 input)
