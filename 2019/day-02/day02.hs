import Debug.Trace (trace)
import Data.List.Split


parse :: String -> [Int]
parse = map read . splitOn ","

repl :: [Int] -> Int -> Int -> [Int]
repl xs a b = (head xs):a:b:(drop 3 xs)

run :: Int -> [Int] -> [Int] -> Int
-- run i _ state | trace ("run " ++ show i ++ " " ++ show state) False = undefined
run i (99:_) state = head state
run i (op:a:b:out:_) state =
    run new_i instr new_state
    where
        opcode 1 = (+)
        opcode 2 = (*)
        r = (opcode op) (state !! a) (state !! b)
        ss = splitAt (out) state
        new_state = fst ss ++ [r] ++ drop 1 (snd ss)
        new_i = i + 4
        instr = drop new_i new_state

part1 :: [Int] -> Int
part1 xs = run 0 xxs xxs
    where xxs = repl xs 12 2

part2 :: [Int] -> Int
part2 xs = head [100 * a + b
                | a <- [0..99],
                  b <- [0..99],
                  let xxs = repl xs a b,
                  let res = run 0 xxs xxs,
                  res == 19690720]

main :: IO ()
main = do
    input <- parse <$> getContents
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
