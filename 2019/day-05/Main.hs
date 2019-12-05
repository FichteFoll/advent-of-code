module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)

theInput = 1

-- ((instr_pointer, code), output)
type IntcodeState = ((Int, [Int]), [Int])

parse :: String -> [Int]
parse = map read . splitOn ","

run :: [Int] -> IntcodeState
run state = head . dropWhile ((/=) (-1) . fst . fst) $ iterate step ((0, state), [])

step :: IntcodeState -> IntcodeState
step ((i, state), out) = case opcode of
  1  -> ((i+4, write 2 $ foldl1 (+) $ take 2 values), out)
  2  -> ((i+4, write 2 $ foldl1 (*) $ take 2 values), out)
  3  -> ((i+2, write 0 theInput), [])
  4  -> ((i+2, state), head values:out)
  99 -> ((-1,  state), out)
  where
    (opmode:params) = drop i state
    (dmodes, opcode) = quotRem opmode 100
    modes = map (flip rem 10 . div dmodes) (iterate (*10) 1)
    values = zipWith loadMode modes params
    loadMode 0 = (!!) state
    loadMode 1 = id
    write = replace state . ((!!) params)

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

part1 :: [Int] -> Int
part1 = head . snd . run

-- part2 :: [Int] -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  -- putStrLn $ "Part 2: " ++ (show $ part2 input)
