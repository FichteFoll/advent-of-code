{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.List.Extra (splitOn)
import Data.Tuple.Extra (both)
import Data.List.Split (chunksOf)
import Data.List (nub)

type Input = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (both read . fmap tail . break (== '-')) . splitOn ","

part1 :: Input -> Int
part1 = solve isInvalid1

part2 :: Input -> Int
part2 = solve isInvalid2

solve :: (Num a, Foldable t, Enum a) => (a -> Bool) -> t (a, a) -> a
solve check = sum . concatMap (filter check . (\t -> [fst t .. snd t]))

isInvalid1 :: Int -> Bool
isInvalid1 n = even l && uncurry (==) (splitAt (l `div` 2) s)
  where
    s = show n
    l = length s

isInvalid2 :: Int -> Bool
isInvalid2 n
  = or
    [ True
    | d <- [1..(l `div` 2)]
    , l `mod` d == 0
    , length (nub $ chunksOf d s) == 1
    ]
  where
    s = show n
    l = length s
