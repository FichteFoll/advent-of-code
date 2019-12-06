module Main where

import Debug.Trace (trace)
import Data.Tree
import Data.Foldable (toList)
import Data.Bifunctor (bimap)
import Control.Applicative (liftA2)

type Input = [(String, String)]

-- depth-first iteration
iterTree :: Tree a -> [Tree a]
iterTree node@(Node _ subforest) = node:(iterForest subforest)
iterForest :: [Tree a] -> [Tree a]
iterForest [] = []
iterForest (node@(Node _ subforest):xs) = node : iterForest subforest ++ iterForest xs

parse :: String -> Input
parse = map (fmap tail . break ((==) ')')) . lines

buildTree :: Input -> Tree String
buildTree xs = unfoldTree (\x -> (x, [b | (a, b) <- xs, a == x])) "COM"

part1 :: Input -> Int
part1 input = sum $ zipWith ((*) . length) (levels tree) [0..]
  where tree = buildTree input

part2 :: Input -> Int
part2 input = findLevel start common + findLevel end common - 2
  where
    tree = buildTree input
    common = last $ filter
      (\node -> node `isAncestor` start && node `isAncestor` end)
      (iterTree tree)

    findLevel test tree = snd . head $ filter (elem test . fst) $ zip (levels tree) [0..]
    isAncestor :: (Eq a) => Tree a -> a -> Bool
    isAncestor (Node name subtrees) test = or $ (name == test):(map (flip isAncestor test) subtrees)
    start = "YOU"
    end = "SAN"

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
