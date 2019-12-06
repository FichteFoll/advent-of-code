module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)

-- ((instr_pointer, code), output)
type IntcodeState = ((Int, [Int]), [Int])

parse :: String -> [Int]
parse = map read . splitOn ","

run :: Int -> [Int] -> IntcodeState
run input state = head . dropWhile ((/=) (-1) . fst . fst) $ iterate (step input) ((0, state), [])

step :: Int -> IntcodeState -> IntcodeState
step input ((i, state), out) = case opcode of
  1  -> ((i+4, write 2 $ binOp (+)), out) -- ADD
  2  -> ((i+4, write 2 $ binOp (*)), out) -- MUL
  3  -> ((i+2, write 0 input), out) -- IN
  4  -> ((i+2, state), head params:out) -- OUT
  5  -> ((jumpIf ((/=) 0), state), out) -- JNZ
  6  -> ((jumpIf ((==) 0), state), out) -- JZ
  7  -> ((i+4, write 2 $ cmp (<)), out) -- LT
  8  -> ((i+4, write 2 $ cmp (==)), out) -- EQ
  99 -> ((-1,  state), out) -- HALT
  where
    (opmode:rawParams) = drop i state
    (dmodes, opcode) = quotRem opmode 100
    modes = map (flip rem 10 . div dmodes) (iterate (*10) 1)
    params = zipWith loadMode modes rawParams
    loadMode 0 = (!!) state
    loadMode 1 = id

    write = replace state . ((!!) rawParams)
    binOp op = foldl1 op $ take 2 params
    jumpIf pred
      | (operand:target:_) <- params, pred operand = target
      | otherwise = i+3
    cmp f = let (a:b:_) = params in fromEnum $ f a b

replace :: [a] -> Int -> a -> [a]
replace xs i v = fst ss ++ [v] ++ drop 1 (snd ss)
  where ss = splitAt i xs

part1 :: [Int] -> Int
part1 = head . snd . (run 1)

part2 :: [Int] -> Int
part2 = head . snd . (run 5)

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
