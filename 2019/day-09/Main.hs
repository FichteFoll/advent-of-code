module Main where

import Data.List.Split (splitOn)
import Data.List
import Control.Applicative (liftA2)

type Tape = [Int]
-- ((instr_pointer, tape), (input, output))
type IntcodeState = ((Int, Tape), ([Int], [Int]))

parse :: String -> Tape
parse = map read . splitOn ","

hasTerminated :: IntcodeState -> Bool
hasTerminated ((-1, _), _) = True
hasTerminated _ = False

needsInput :: IntcodeState -> Bool
needsInput ((i, tape), (inp, _)) = (tape !! i) == 3 && null inp

run :: IntcodeState -> [Int] -> IntcodeState
run (state, (oldInp, out)) input =
  head . dropWhile (not . liftA2 (||) hasTerminated needsInput)
  $ iterate step (state, (oldInp ++ input, []))

step :: IntcodeState -> IntcodeState
step ((i, tape), io@(inp, out)) = case opcode of
  1  -> ((i+4, write 2 $ binOp (+)), io) -- ADD
  2  -> ((i+4, write 2 $ binOp (*)), io) -- MUL
  3  -> ((i+2, write 0 $ head inp), (tail inp, out)) -- IN
  4  -> ((i+2, tape), (inp, out ++ [head params])) -- OUT
  5  -> ((jumpIf ((/=) 0), tape), io) -- JNZ
  6  -> ((jumpIf ((==) 0), tape), io) -- JZ
  7  -> ((i+4, write 2 $ binCmp (<)), io) -- LT
  8  -> ((i+4, write 2 $ binCmp (==)), io) -- EQ
  99 -> ((-1, tape), io) -- HALT
  where
    (opmode:rawParams) = drop i tape
    (dmodes, opcode) = quotRem opmode 100
    modes = map (flip rem 10 . div dmodes) (iterate (*10) 1)
    params = zipWith loadMode modes rawParams
    loadMode 0 = (!!) tape
    loadMode 1 = id

    write = replace tape . ((!!) rawParams)
    binOp op = foldl1 op $ take 2 params
    jumpIf pred
      | (operand:target:_) <- params, pred operand = target
      | otherwise = i+3
    binCmp f = fromEnum $ f a b where (a:b:_) = params

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

newAmp :: Tape -> [Int] -> IntcodeState
newAmp tape input = ((0, tape), (input, []))

initAmps :: Tape -> [Int] -> [IntcodeState]
initAmps tape phases = [newAmp tape [phase] | phase <- phases]

-- First part of the tuple carries over output
runSeq :: ([Int], [IntcodeState]) -> ([Int], [IntcodeState])
runSeq (input, []) = (input, [])
runSeq (input, (amp:amps)) = (nextInput, nextAmp:newAmps)
  where
    afterRun@(_, (_, out)) = run amp input
    nextAmp = fmap ([] <$) afterRun
    (nextInput, newAmps) = runSeq (out, amps)

computeSeqLoop :: Tape -> [Int] -> Int
computeSeqLoop tape phases = last $ fst $ head
  $ dropWhile (not . hasTerminated . last . snd)
  $ iterate runSeq ([0], initAmps tape phases)

part1 :: Tape -> Int
part1 tape = maximum $ map (computeSeqLoop tape) $ permutations [0..4]

part2 :: Tape -> Int
part2 tape = maximum $ map (computeSeqLoop tape) $ permutations [5..9]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
