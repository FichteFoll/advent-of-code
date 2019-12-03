module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)


parse :: String -> [Int]
parse = map read . splitOn ","

repl :: [Int] -> Int -> Int -> [Int]
repl (a:_:_:xs) b c = a:b:c:xs

run :: Int -> [Int] -> [Int]
run i state = case drop i state of
    (99:_) -> state
    (op:a:b:out:_) ->
        run new_i new_state
        where
            r = (opcode op) (state !! a) (state !! b)
            ss = splitAt out state
            new_state = fst ss ++ [r] ++ drop 1 (snd ss)
            new_i = i + 4
    where
        opcode 1 = (+)
        opcode 2 = (*)

part1 :: [Int] -> Int
part1 xs = head $ run 0 $ repl xs 12 2

part2 :: [Int] -> Int
part2 xs = head [100 * n + v
                | n <- [0..99]
                , v <- [0..99]
                , (head . run 0 $ repl xs n v) == 19690720]

main :: IO ()
main = do
    input <- parse <$> getContents
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
