module Main where

import Data.List.Split (splitOn)
import Data.List
import Control.Applicative (liftA2)

-- ((instr_pointer, code), (input, output))
type IntcodeState = ((Int, [Int]), ([Int], [Int]))

parse :: String -> [Int]
parse = map read . splitOn ","

hasTerminated :: IntcodeState -> Bool
hasTerminated ((-1, _), _) = True
hasTerminated _ = False

needsInput :: IntcodeState -> Bool
needsInput ((i, code), (inp, _)) = (code !! i) == 3 && null inp

run :: IntcodeState -> [Int] -> IntcodeState
run (state, (oldInp, out)) input =
  head . dropWhile (not . liftA2 (||) hasTerminated needsInput)
  $ iterate step (state, (oldInp ++ input, []))

step :: IntcodeState -> IntcodeState
step ((i, code), state@(inp, out)) = case opcode of
  1  -> ((i+4, write 2 $ binOp (+)), state) -- ADD
  2  -> ((i+4, write 2 $ binOp (*)), state) -- MUL
  3  -> ((i+2, write 0 $ head inp), (tail inp, out)) -- IN
  4  -> ((i+2, code), (inp, out ++ [head params])) -- OUT
  5  -> ((jumpIf ((/=) 0), code), state) -- JNZ
  6  -> ((jumpIf ((==) 0), code), state) -- JZ
  7  -> ((i+4, write 2 $ binCmp (<)), state) -- LT
  8  -> ((i+4, write 2 $ binCmp (==)), state) -- EQ
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
    binCmp f = fromEnum $ f a b where (a:b:_) = params

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

newAmp :: [Int] -> [Int] -> IntcodeState
newAmp code input = ((0, code), (input, []))

initAmps :: [Int] -> [Int] -> [IntcodeState]
initAmps code phases = [newAmp code [phase] | phase <- phases]

-- First part of the tuple carries over output
runSeq :: ([Int], [IntcodeState]) -> ([Int], [IntcodeState])
runSeq (input, []) = (input, [])
runSeq (input, (amp:amps)) = (nextInput, nextAmp:newAmps)
  where
    afterRun@(_, (_, out)) = run amp input
    nextAmp = fmap ([] <$) afterRun
    (nextInput, newAmps) = runSeq (out, amps)

computeSeqLoop :: [Int] -> [Int] -> Int
computeSeqLoop code phases = last $ fst $ head
  $ dropWhile (not . hasTerminated . last . snd)
  $ iterate runSeq ([0], initAmps code phases)

part1 :: [Int] -> Int
part1 code = maximum $ map (computeSeqLoop code) $ permutations [0..4]

part2 :: [Int] -> Int
part2 code = maximum $ map (computeSeqLoop code) $ permutations [5..9]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
