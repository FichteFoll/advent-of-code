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
countSteps net pos (lr:lrs) = 1 + countSteps net (pos' net) lrs
  where
    pos' ((key, branch):net')
      | key == pos = getter lr branch
      | otherwise = pos' net'
    getter 'L' = fst
    getter 'R' = snd

part2 :: Input -> Int
part2 _ = 0
