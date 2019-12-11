module Main where

import Control.Applicative (liftA2)
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

import Linear.V2


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

-------------------------------------------------------------------------------

-- Prefix with D to prevent collision with Either constructors
data Direction = DUp | DRight | DDown | DLeft deriving (Bounded, Eq, Enum)
type Point = V2 Int
data Color = Black | White deriving (Enum, Eq)

toV2 :: Direction -> V2 Int
toV2 DUp = V2 0 1
toV2 DRight = V2 1 0
toV2 DDown = V2 0 (-1)
toV2 DLeft = V2 (-1) 0

turn :: Direction -> Direction -> Direction
turn a DLeft  = if a == minBound then maxBound else pred a
turn a DRight = if a == maxBound then minBound else succ a
turn a _ = undefined

paint :: IntcodeState -> Point -> Direction -> Map Point Color -> Map Point Color
paint amp pos dir painted
  | hasTerminated amp = painted
  | otherwise = paint newAmp newPos newDir newPainted
  where
    inColor = fromEnum $ Map.findWithDefault Black pos painted
    ampWithOutput = run amp [inColor]
    newAmp = ampWithOutput { output = [] }
    (outColor:outTurn:_) = output ampWithOutput
    newDir = turn dir (if outTurn == 0 then DLeft else DRight)
    newPos = pos + toV2 newDir
    newPainted = Map.insert pos (toEnum outColor) painted

part1 :: Tape -> Int
part1 intape = length $ paint (newAmp {tape = intape}) (pure 0) DUp Map.empty

part2 :: Tape -> String
part2 intape = unlines [[if elem (V2 x y) whites then '█' else ' '
                        | x <- [minimum xs..maximum xs]]
                       | y <- [minimum ys..maximum ys]]
  where
    painted = paint (newAmp {tape = intape}) (pure 0) DUp $ Map.singleton (pure 0) White
    whites = Map.keys $ Map.filter (== White) painted
    (xs, ys) = unzip $ map (\(V2 x y) -> (x,y)) whites

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2:\n" ++ part2 input
