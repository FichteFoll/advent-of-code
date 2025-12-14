{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.Set as S
import Linear.V2

type Input = (Int, S.Set (V2 Int))

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse grid = (dim, S.fromList [V2 x y | (i, c) <- zip [0 ..] $ concat rows, c == '@', let (y, x) = i `divMod` dim])
  where
    rows = lines grid
    dim = length rows

part1 :: Input -> Int
part1 (_, rolls) = length $ filter ((< 4) . length . filter (`S.member` rolls) . neighbors) $ S.toList rolls

part2 :: Input -> Int
part2 _ = 0

neighbors :: V2 Int -> [V2 Int]
neighbors v = [v + V2 x y | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]
