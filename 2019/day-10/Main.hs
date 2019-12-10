module Main where

import Data.List
import Control.Applicative (liftA2)

type Asteroid = (Double, Double)
data AsteroidRel = Ast {
  ast :: Asteroid,
  angle :: Double,
  dist :: Double
  } deriving (Eq, Show)

parse :: String -> [Asteroid]
parse input = concat [[(x, y) | (c, x) <- zip line [0..], c == '#']
                      | (line, y) <- zip (lines input) [0..]]

monitor :: [Asteroid] -> (Asteroid, Int)
monitor pts = last $ sortOn snd $ map ptcount pts
  where
    ptcount pt = (pt, length $ nub $ map (uncurry atan2 . (translate pt)) pts)

translate :: (Num a) => (a,a) -> (a,a) -> (a,a)
translate (x1,y1) (x2,y2) = (x2-x1,y2-y1)

-- sorted and grouped asteroids by angle and then distance
angles :: Asteroid -> [Asteroid] -> [[AsteroidRel]]
angles origin pts =
  map (sortOn dist)
  $ groupBy (\a b -> angle a == angle b)
  $ sortOn angle
  $ [let relPt = translate origin pt in
     Ast pt (cAngle relPt) (manhattan relPt)
    | pt <- pts, pt /= origin]
  where
    cAngle (x,y) = asc $ atan2 x (-y)
    asc a | a < 0 = a + 2*pi
          | otherwise = a
    manhattan (x,y) = (abs x) + (abs y)

vaporize :: [[AsteroidRel]] -> [AsteroidRel]
vaporize [] = []
vaporize ((next:[]):asts) = next:(vaporize asts)
vaporize ((next:rem):asts) = next:(vaporize $ asts ++ [rem])

part1 :: [Asteroid] -> Int
part1 = snd . monitor

part2 :: [Asteroid] -> Int
part2 pts = round $ x * 100 + y
  where
    origin = fst $ monitor pts
    Ast{ast = (x, y)} = vaporize (angles origin pts) !! 199

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
