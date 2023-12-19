{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Control.Arrow ((***))
import Data.List.Extra (dropEnd)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int}
data Rule = Rule { cond :: Part -> Bool, target :: String}
type Workflows = Map String [Rule]

type Input = (Workflows, [Part])

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = (Map.fromList . map parseWorkflow *** map parsePart . tail) . break null . lines

parsePart :: String -> Part
parsePart line = Part x' m' a' s'
  where [x', m', a', s'] = map (read . drop 2) . splitOn "," . dropBoth 1 $ line

parseWorkflow :: String -> (String, [Rule])
parseWorkflow = fmap (map parseRule . splitOn ","  . dropBoth 1) . break (== '{')

parseRule :: String -> Rule
parseRule target | ':' `notElem` target = Rule (const True) target
parseRule str = Rule cond target
  where
    cond = flip (op opc) (read rest) . lens field
    (field:opc:rest, target) = tail <$> break (== ':') str
    op '>' = (>)
    op '<' = (<)
    op _ = error str
    lens 'x' = x
    lens 'm' = m
    lens 'a' = a
    lens 's' = s
    lens _ = undefined

part1 :: Input -> Int
part1 (ws, ps) = sum . map partSum $ filter (isAccepted ws) ps

part2 :: Input -> Int
part2 _ = 0

partSum :: Part -> Int
partSum p = sum $ map ($ p) [x, m, a, s]

isAccepted :: Workflows -> Part -> Bool
isAccepted ws p = go "in"
  where
    go "A" = True
    go "R" = False
    go k = go $ target $ head $ filter (`cond` p) (ws ! k)

dropBoth n = drop n . dropEnd n
