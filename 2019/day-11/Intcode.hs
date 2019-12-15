module Intcode where

import Control.Applicative (liftA2)
import Control.DeepSeq (force)
import Data.List
import Data.List.Split (splitOn)

type Tape = [Int]
data IntcodeMachine = IM {
  pointer :: Int,
  tape :: Tape,
  relOffset :: Int,
  input :: [Int],
  output :: [Int]
} deriving (Show)

newIM :: IntcodeMachine
newIM = IM 0 [] 0 [] []

hasTerminated :: IntcodeMachine -> Bool
hasTerminated IM{pointer = (-1)} = True
hasTerminated _ = False

needsInput :: IntcodeMachine -> Bool
needsInput s = (tape s !! pointer s) == 3 && null (input s)

run :: IntcodeMachine -> [Int] -> IntcodeMachine
run s newInput =
  head . dropWhile (not . liftA2 (||) hasTerminated needsInput)
  $ iterate step $ s {input = input s ++ newInput}

step :: IntcodeMachine -> IntcodeMachine
step s@IM{ pointer = i, tape = tape } = case opcode of
  1  -> s { pointer = i+4, tape = write 2 $ binOp (+) } -- ADD
  2  -> s { pointer = i+4, tape = write 2 $ binOp (*) } -- MUL
  3  -> s { pointer = i+2, tape = write 0 $ head (input s), input = tail $ input s } -- IN
  4  -> s { pointer = i+2, output = output s ++ take 1 params } -- OUT
  5  -> s { pointer = jumpIf (/= 0) } -- JNZ
  6  -> s { pointer = jumpIf (== 0) } -- JZ
  7  -> s { pointer = i+4, tape = write 2 $ binCmp (<) } -- LT
  8  -> s { pointer = i+4, tape = write 2 $ binCmp (==) } -- EQ
  9  -> s { pointer = i+2, relOffset = relOffset s + head params } -- SET_RBO
  99 -> s { pointer = -1 } -- HALT
  where
    (opmode:rawParams) = drop i tape
    (dmodes, opcode) = quotRem opmode 100
    modes = map (flip rem 10 . div dmodes) (iterate (*10) 1)
    params = zipWith loadMode modes rawParams
    writeParams = zipWith loadWriteMode modes rawParams
    loadMode 0 = read
    loadMode 1 = id
    loadMode 2 = read . (+) (relOffset s)
    loadWriteMode 0 = id
    loadWriteMode 2 = (+) (relOffset s)

    read pos | pos < length tape = tape !! pos
             | otherwise = 0
    -- Force deep evaluation to reduce thunk collection
    write pIdx val | pos < length tape = force $ replace tape pos val
                   | otherwise = force $ tape ++ replicate (pos - length tape) 0 ++ [val]
                   where pos = writeParams !! pIdx
    binOp op = foldl1 op $ take 2 params
    jumpIf f | (operand:target:_) <- params, f operand = target
             | otherwise = i+3
    binCmp f = fromEnum $ f a b where (a:b:_) = params

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

parse :: String -> Tape
parse = map read . splitOn ","
