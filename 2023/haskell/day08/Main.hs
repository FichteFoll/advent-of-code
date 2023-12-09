{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main, parse, part1, part2) where
import Control.Arrow ((&&&))

type Network = [(String, (String, String))]
type Input = (String, Network)

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = (head &&& map parseMapLine . drop 2) . lines
  where
    parseMapLine = take 3 &&& (take 3 . drop 7 &&& take 3 . drop 12)

part1 :: Input -> Int
part1 (lrs, net) = countSteps net "AAA" (cycle lrs)

countSteps :: Network -> String -> [Char] -> Int
countSteps _ "ZZZ" _ = 0
countSteps net pos (lr:lrs) = 1 + countSteps net (nextPos net lr pos) lrs

part2 :: Input -> Int
part2 (lrs, net) = countSteps2 net start (cycle lrs)
  where
    start = [pos | (pos, _) <- net, last pos == 'A']

countSteps2 :: Network -> [String] -> [Char] -> Int
countSteps2 _ state _ | all ((== 'Z') . last) state = 0
countSteps2 net state (lr:lrs) = 1 + countSteps2 net state' lrs
  where
    state' = map (nextPos net lr) state

nextPos :: Network -> Char -> String -> String
nextPos ((key, branch):net') lr pos
  | key == pos = getter lr branch
  | otherwise = nextPos net' lr pos
  where
    getter 'L' = fst
    getter 'R' = snd
