{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2, countCombinations) where

import Data.List.Split (splitWhen)
import Control.Arrow ((***))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Control.Lens (over, both)
import Debug.Trace

type Input = [(String, [Int])]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map parseLine . lines
  where parseLine = fmap (map read . splitWhen (== ',') . tail) . break (== ' ')

part1 :: Input -> Int
-- part1 = sum . map solveLine
part1 = sum . map ((uncurry .) countCombinations Nothing)

part2 :: Input -> Int
-- part2 = part1 . map (unfold 5)
part2 = sum . map (solveLine . unfold 5)
  where unfold n = intercalate "?" . replicate n *** concat . replicate n

countCombinations :: Maybe Int -> String -> [Int] -> Int
-- carry, line, counts
countCombinations Nothing  []       []     = 1
countCombinations (Just 0) []       []     = 1
countCombinations _        []       _      = 0
countCombinations Nothing  ('.':cs) ns     = countCombinations Nothing cs ns
countCombinations (Just 0) ('.':cs) ns     = countCombinations Nothing cs ns
countCombinations _        ('.':_)  _      = 0
countCombinations Nothing  ('#':_)  []     = 0
countCombinations Nothing  ('#':cs) (n:ns) = countCombinations (Just $ pred n) cs ns
countCombinations Nothing  ('?':cs) (n:ns) = countCombinations (Just $ pred n) cs ns + countCombinations Nothing cs (n:ns)
countCombinations Nothing  ('?':cs) ns     = countCombinations Nothing cs ns
countCombinations (Just 0) ('#':_)  _      = 0
countCombinations (Just 0) ('?':cs) ns     = countCombinations Nothing cs ns
countCombinations (Just n) ('#':cs) ns     = countCombinations (Just $ pred n) cs ns
countCombinations (Just n) ('?':cs) ns     = countCombinations (Just $ pred n) cs ns
countCombinations carry cs ns
  = error $ "not covered: " ++ show carry ++ " " ++ cs ++ " " ++ show ns

solveLine :: (String, [Int]) -> Int
solveLine (line, ns) = traceShowId $ countCombinations' Map.empty [k] ! k
  where k = (Nothing, line, ns)

type Key = (Maybe Int, String, [Int])
type Cache = Map Key Int

countCombinations' :: Cache -> [Key] -> Cache
countCombinations' cache [] = cache
countCombinations' cache (k:ks)
  | k `Map.member` cache = countCombinations' cache ks
  | otherwise = case k of
    (Nothing, []    , []  ) -> resolve 1
    (Just 0 , []    , []  ) -> resolve 1
    (_      , []    , _   ) -> resolve 0
    (Nothing, '.':cs, ns  ) -> recurse (Nothing, cs, ns)
    (Just 0 , '.':cs, ns  ) -> recurse (Nothing, cs, ns)
    (_      , '.':_ , _   ) -> resolve 0
    (Nothing, '#':_ , []  ) -> resolve 0
    (Nothing, '#':cs, n:ns) -> recurse (Just (pred n), cs, ns)
    (Nothing, '?':cs, n:ns) -> branch  (Just (pred n), cs, ns) (Nothing, cs, n:ns)
    (Nothing, '?':cs, []  ) -> recurse (Nothing, cs, [])
    (Just 0 , '#':_ , _   ) -> resolve 0
    (Just 0 , '?':cs, ns  ) -> recurse (Nothing, cs, ns)
    (Just n , '#':cs, ns  ) -> recurse (Just (pred n), cs, ns)
    (Just n , '?':cs, ns  ) -> recurse (Just (pred n), cs, ns)
    (carry, cs, ns) -> error $ "not covered: " ++ show carry ++ " " ++ cs ++ " " ++ show ns
  where
    resolve n = countCombinations' (Map.insert k n cache) ks
    recurse k' = case Map.lookup k' cache of
      Just n  -> resolve n
      Nothing -> recurse' k'
    recurse' k' = countCombinations' cache (k':ks ++ [k])
    branch k' k'' = case over both (`Map.lookup` cache) (k', k'') of
      (Nothing, _) -> recurse' k'
      (_, Nothing) -> recurse' k''
      (Just n', Just n'') -> resolve $ n' + n''
