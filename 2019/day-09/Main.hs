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
    write pIdx val | pos < length tape = replace tape pos val
                  | otherwise = tape ++ replicate (pos - length tape) 0 ++ [val]
                  where pos = writeParams !! pIdx
    binOp op = foldl1 op $ take 2 params
    jumpIf pred | (operand:target:_) <- params, pred operand = target
                | otherwise = i+3
    binCmp f = fromEnum $ f a b where (a:b:_) = params

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

part1 :: Tape -> [Int]
part1 intape = output $ run (newAmp {tape = intape}) [1]

part2 :: Tape -> [Int]
part2 intape = output $ run (newAmp {tape = intape}) [2]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
