{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main (main, parse, part1, part2) where

import Control.Arrow ((***))
import Control.Lens (makeLenses, (^.), (.~), Lens')
import Data.Either (lefts)
import Data.List.Extra (dropEnd)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Function ((&))

data Part = Part { _x :: Int, _m :: Int, _a :: Int, _s :: Int}
$(makeLenses ''Part)
data Cond = Cond { l :: Lens' Part Int, opc :: Char, n :: Int }
data Rule = Rule { cond :: Maybe Cond, target :: String}
type Workflows = Map String [Rule]
data State = State { _pos :: String, _start :: Part, _end :: Part }
$(makeLenses ''State)

type Input = (Workflows, [Part])

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = (Map.fromList . map parseWorkflow *** map parsePart . tail) . break null . lines

part1 :: Input -> Int
part1 (ws, ps) = sum . map partSum $ filter (isAccepted ws) ps

part2 :: Input -> Int
part2 (ws, _)
  = sum
    . map countCombs
    . filter ((== "A") . (^. pos))
    $ allCombs ws [State "in" (Part 1 1 1 1) (Part 4000 4000 4000 4000)]

parsePart :: String -> Part
parsePart line = Part x' m' a' s'
  where [x', m', a', s'] = map (read . drop 2) . splitOn "," . dropBoth 1 $ line

parseWorkflow :: String -> (String, [Rule])
parseWorkflow = fmap (map parseRule . splitOn ","  . dropBoth 1) . break (== '{')

parseRule :: String -> Rule
parseRule target | ':' `notElem` target = Rule Nothing target
parseRule str = Rule (Just cond) target
  where
    cond = Cond (lens' field) opc (read rest)
    (field:opc:rest, target) = tail <$> break (== ':') str
    lens' 'x' = x
    lens' 'm' = m
    lens' 'a' = a
    lens' 's' = s
    lens' _ = undefined

dropBoth n = drop n . dropEnd n

partSum :: Part -> Int
partSum = sum . flip map [x, m, a, s] . (^.)

isAccepted :: Workflows -> Part -> Bool
isAccepted ws p = go "in"
  where
    go "A" = True
    go "R" = False
    go k = go $ target $ head $ filter (eval . cond) (ws ! k)
    eval (Just c) = flip (op $ opc c) (n c) $ p ^. l c
    eval Nothing = True
    op '>' = (>)
    op '<' = (<)
    op _ = undefined

-- Track states of a pair of Parts (denoting min and max for each score) and the current position.
-- For a rule, splitting over each condition (in order).
-- When rule is A, add combinations to total count.
-- When rule is R, abort (add 0 to total count).
-- If condition can match, split state into part that can match and pass part that can't into the next rule.
-- If condition cannot match, pass to the next rule.
allCombs :: Workflows -> [State] -> [State]
allCombs _ [] = []
allCombs ws (s:ss)
  | s ^. pos `elem` ["A", "R"] = s : allCombs ws ss
  | otherwise = allCombs ws (branches ++ ss)
  where
    branches = concatMap lefts $ scanl branch [Right s] $ ws ! (s ^. pos)
    -- left is result, right is carry
    branch :: [Either State State] -> Rule -> [Either State State]
    branch [] _ = []
    branch (Left  _:ss) r = branch ss r
    branch (Right s:ss) r@(Rule Nothing target) = Left (s & pos .~ target) : branch ss r
    branch (Right s:ss) r@(Rule (Just (Cond l opc n)) target)
      | opc == '<' && high < n = Left s' : branch ss r
      | opc == '>' && n < low  = Left s' : branch ss r
      | opc == '<' && low < n && n < high = Left (s' & (end . l) .~ pred n) : Right (s & (start . l) .~ n) : branch ss r
      | opc == '>' && low < n && n < high = Left (s' & (start . l) .~ succ n) : Right (s & (end . l) .~ n) : branch ss r
      | otherwise = Right s' : branch ss r
      where
        s' = s & pos .~ target
        low = s ^. (start . l)
        high = s ^. (end . l)

countCombs :: State -> Int
countCombs st = product $ map size [x, m, a, s]
  where size lens = succ $ st ^. (end . lens) - st ^. (start . lens)
