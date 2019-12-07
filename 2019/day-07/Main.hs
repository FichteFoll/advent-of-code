module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List

-- ((instr_pointer, code), (input, output))
type IntcodeState = ((Int, [Int]), ([Int], [Int]))

parse :: String -> [Int]
parse = map read . splitOn ","

run :: [Int] -> [Int] -> IntcodeState
run code input = 
  head . dropWhile ((/=) (-1) . fst . fst)
  $ iterate (step) ((0, code), (input, []))

step :: IntcodeState -> IntcodeState
step ((i, code), state@(inp, out)) = case opcode of
  1  -> ((i+4, write 2 $ binOp (+)), state) -- ADD
  2  -> ((i+4, write 2 $ binOp (*)), state) -- MUL
  3  -> ((i+2, write 0 $ head inp), (tail inp, out)) -- IN
  4  -> ((i+2, code), (inp, head params:out)) -- OUT
  5  -> ((jumpIf ((/=) 0), code), state) -- JNZ
  6  -> ((jumpIf ((==) 0), code), state) -- JZ
  7  -> ((i+4, write 2 $ cmp (<)), state) -- LT
  8  -> ((i+4, write 2 $ cmp (==)), state) -- EQ
  99 -> ((-1, code), state) -- HALT
  where
    (opmode:rawParams) = drop i code
    (dmodes, opcode) = quotRem opmode 100
    modes = map (flip rem 10 . div dmodes) (iterate (*10) 1)
    params = zipWith loadMode modes rawParams
    loadMode 0 = (!!) code
    loadMode 1 = id

    write = replace code . ((!!) rawParams)
    binOp op = foldl1 op $ take 2 params
    jumpIf pred
      | (operand:target:_) <- params, pred operand = target
      | otherwise = i+3
    cmp f = fromEnum $ f a b where (a:b:_) = params

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

runSeq :: [Int] -> Int -> [Int] -> Int
runSeq code input [] = input
runSeq code input (phase:seq) = runSeq code carry seq
  where
    output = head . snd . snd
    carry = output $ run code [phase, input]

part1 :: [Int] -> Int
part1 code = maximum $ map (runSeq code 0) $ permutations [0..4]

-- part2 :: [Int] -> Int
-- part2 = head . snd . snd . (run $ repeat 5)

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  -- putStrLn $ "Part 2: " ++ (show $ part2 input)
