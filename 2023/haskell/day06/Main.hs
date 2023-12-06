module Main (main, parse, part1, part2) where

import Control.Applicative (liftA2)
import Control.Lens
import Data.List (singleton)

-- (time, distance)
type Input = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = uncurry zip . over both (map read . tail . words) . fmap tail . break (== '\n')

part1 :: Input -> Int
part1 = product . map numWins
  where
    numWins (xMax, yMin) = succ $ uncurry subtract $ bimap ceiling floor $ deQuad (negate xMax) (succ yMin)
    deQuad p q = over both (subtract (p' / 2)) $ liftA2 (,) negate id $ sqrt $ (p' / 2)^2 - q'
      where
        p' = fromIntegral p
        q' = fromIntegral q

part2 :: Input -> Int
part2 = part1 . singleton . over both (read . concatMap show) . unzip

{-
  time: xMax
  distance: yMin
  y = x * (xMax - x) > yMin, 0 < x < xMax
  yMin + 1 = x * xMax - x * x
  0 = x^2 - xMax * x + (yMin + 1)
  -- https://en.wikipedia.org/wiki/Quadratic_equation#Reduced_quadratic_equation
  x = xMax/2 +- sqrt((-xMax/2)^2 - (yMin + 1))
-}
