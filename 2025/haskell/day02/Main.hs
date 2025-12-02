{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.List.Extra (splitOn)
import Data.Tuple.Extra (both)

type Input = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (both read . fmap tail . break (== '-')) . splitOn ","

part1 :: Input -> Int
part1 = sum . concatMap (filter isInvalid . (\t -> [fst t .. snd t]))

isInvalid :: Int -> Bool
isInvalid n = even l && uncurry (==) (splitAt (l `div` 2) s)
  where
    s = show n
    l = length s

part2 :: Input -> Int
part2 _ = 0
