{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main, parse, part1, part2) where

import Data.List.NonEmpty (groupWith)
import Control.Arrow

type Input = [Int]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (liftA2 ($) (op . head) (read . tail)) . lines
  where
    op 'L' = negate
    op 'R' = id

part1 :: Input -> Int
part1 = length . filter ((== 0) . flip mod 100) . scanl (+) 50

-- Group continuous movements into the same direction
-- to ensure that the added offsets are balanced,
-- while ensuring that the first movement is always 'leftward'.
-- Then count separately the number of times we stop at 0
-- and the times we pass zero,
-- while diminishing rightward movement by 1
-- to not count landing on a 0 as passing it.
part2 :: Input -> Int
part2
  = uncurry (+)
  . (countZerosPassed &&& countZeroStops)
  . scanl (+) 50
  . map sum
  . groupWith signum'
  . (0 :)
  where
    signum' 0 = -1
    signum' n = signum n
    countZeroStops = length . filter ((== 0) . (`mod` 100))
    countZerosPassed
      = sum
      . map abs
      . (zipWith (-) =<< tail)
      . map (`div` 100)
      . zipWith (+) (0 : cycle [0, -1])
