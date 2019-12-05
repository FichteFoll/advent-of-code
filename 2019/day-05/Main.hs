module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)

theInput = 1

parse :: String -> [Int]
parse = map read . splitOn ","

run :: Int -> [Int] -> [Int] -> ([Int], [Int])
run i state out = case runInstr opcode modes params state of
  (0, _, _) -> (state, out)
  (offset, newState, addOut) -> run (i + offset) newState (addOut ++ out)
  where
    (opmode:params) = drop i state
    (modes, opcode) = quotRem opmode 100

runInstr :: Int -> Int -> [Int] -> [Int] -> (Int, [Int], [Int])
runInstr opcode modes params state
  | opcode == 99 = (0, state, [])
  | elem opcode [1..2] =
    let newState = replace state (writeParam 2) $ (binop opcode) (param 0) (param 1)
        binop 1 = (+)
        binop 2 = (*)
    in (4, newState, [])
  | opcode == 3 = (2, replace state (writeParam 0) theInput, [])
  | opcode == 4 = (2, state, [param 0])
  where
    mode pi = flip rem 10 $ modes `div` (iterate (*10) 1 !! pi)
    param pi = case mode pi of
      0 -> (!!) state . (!!) params $ pi
      1 -> (!!) params $ pi
    writeParam = (!!) params

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
