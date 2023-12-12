{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

type Input = [[Int]]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (map read . words) . lines

part1 :: Input -> Int
part1 = sum . map nextNum

part2 :: Input -> Int
part2 = sum . map (nextNum . reverse)

nextNum :: [Int] -> Int
nextNum ns
  | all (== 0) ns = 0
  | otherwise = last ns + nextNum (zipWith (-) =<< tail $ ns)
