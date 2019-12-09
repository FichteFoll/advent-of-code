module Main where

import Data.List.Split (splitOn)
import Data.List
import Control.Applicative (liftA2)

type Tape = [Int]
data IntcodeState = IState {
  pointer :: Int,
  tape :: Tape,
  relOffset :: Int,
  input :: [Int],
  output :: [Int]
} deriving (Show)

newAmp :: IntcodeState
newAmp = IState 0 [] 0 [] []

parse :: String -> Tape
parse = map read . splitOn ","

hasTerminated :: IntcodeState -> Bool
hasTerminated IState{pointer = (-1)} = True
hasTerminated _ = False

needsInput :: IntcodeState -> Bool
needsInput s = ((tape s) !! (pointer s)) == 3 && null (input s)

run :: IntcodeState -> [Int] -> IntcodeState
run s newInput =
  head . dropWhile (not . liftA2 (||) hasTerminated needsInput)
  $ iterate step $ s {input = input s ++ newInput}

step :: IntcodeState -> IntcodeState
step s@IState{ pointer = i, tape = tape } = case opcode of
  1  -> s { pointer = i+4, tape = write 2 $ binOp (+) } -- ADD
  2  -> s { pointer = i+4, tape = write 2 $ binOp (*) } -- MUL
  3  -> s { pointer = i+2, tape = write 0 $ head (input s), input = tail $ input s } -- IN
  4  -> s { pointer = i+2, output = output s ++ take 1 params } -- OUT
  5  -> s { pointer = jumpIf ((/=) 0) } -- JNZ
  6  -> s { pointer = jumpIf ((==) 0) } -- JZ
  7  -> s { pointer = i+4, tape = write 2 $ binCmp (<) } -- LT
  8  -> s { pointer = i+4, tape = write 2 $ binCmp (==) } -- EQ
  -- 9  -> TODO -- RBO (relative base offset)
  99 -> s { pointer = -1 } -- HALT
  where
    (opmode:rawParams) = drop i tape
    (dmodes, opcode) = quotRem opmode 100
    modes = map (flip rem 10 . div dmodes) (iterate (*10) 1)
    params = zipWith loadMode modes rawParams
    loadMode 0 = (!!) tape
    loadMode 1 = id

    -- TODO increase size if not big enough
    write = replace tape . ((!!) rawParams)
    binOp op = foldl1 op $ take 2 params
    jumpIf pred
      | (operand:target:_) <- params, pred operand = target
      | otherwise = i+3
    binCmp f = fromEnum $ f a b where (a:b:_) = params

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

initAmps :: Tape -> [Int] -> [IntcodeState]
initAmps tape phases = [newAmp {tape = tape, input = [phase]} | phase <- phases]

-- First part of the tuple carries over output
runSeq :: ([Int], [IntcodeState]) -> ([Int], [IntcodeState])
runSeq (input, []) = (input, [])
runSeq (input, (amp:amps)) = (newInput, newAmp:newAmps)
  where
    afterRun = run amp input
    (newInput, newAmps) = runSeq (output afterRun, amps)
    newAmp = afterRun {output = []}

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
