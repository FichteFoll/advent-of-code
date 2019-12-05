module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)

theInput = 1

parse :: String -> [Int]
parse = map read . splitOn ","

run :: Int -> [Int] -> [Int] -> ([Int], [Int])
run i state out = case runInstr opcode dmodes params state of
  (0, _, _) -> (state, out)
  (offset, newState, addOut) -> run (i + offset) newState (addOut ++ out)
  where
    (opmode:params) = drop i state
    (dmodes, opcode) = quotRem opmode 100

runInstr :: Int -> Int -> [Int] -> [Int] -> (Int, [Int], [Int])
runInstr opcode dmodes params state = case opcode of
  1  -> (4, write 2 $ foldl1 (+) $ take 2 values, [])
  2  -> (4, write 2 $ foldl1 (*) $ take 2 values, [])
  3  -> (2, write 0 theInput, [])
  4  -> (2, state, [head values])
  99 -> (0, state, [])
  where
    modes = map (flip rem 10 . div dmodes) (iterate (*10) 1)
    values = zipWith loadMode modes params
    loadMode 0 = (!!) state
    loadMode 1 = id
    write = replace state . ((!!) params)

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

part1 :: [Int] -> Int
part1 xs = head . snd $ run 0 xs []

-- part2 :: [Int] -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  -- putStrLn $ "Part 2: " ++ (show $ part2 input)
