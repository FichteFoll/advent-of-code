module Bench (main) where

import Criterion.Main
import Main hiding (main)

main = do
  inputString <- getContents
  let input = parse inputString
  defaultMain
    [ bench "parse" $ whnf parse inputString
    , bench "part1" $ whnf part1 input
    , bench "part2" $ whnf part2 input]
