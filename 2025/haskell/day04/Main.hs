{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.Set as S
import Linear.V2

-- First element only for parsing assertions.
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
part1 = length . findRemovables . snd

part2 :: Input -> Int
part2 (_, rolls) = length rolls - numRemoved
  where numRemoved = fst $ head $ dropWhile (uncurry (/=)) $ (zip =<< tail) $ map length $ iterate (\s -> s S.\\ findRemovables s) rolls

neighbors :: V2 Int -> [V2 Int]
neighbors v = [v + V2 x y | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]

findRemovables :: S.Set (V2 Int) -> S.Set (V2 Int)
findRemovables rolls = S.filter ((< 4) . length . filter (`S.member` rolls) . neighbors) rolls
