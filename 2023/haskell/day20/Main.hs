{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2, pulseProduct, push) where

import Control.Lens
import Data.Graph (Graph, graphFromEdges, Vertex, vertices, edges)
import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, isJust)
import Distribution.Utils.Generic (sndOf3)
import qualified Data.Map.Strict as Map

data Node = Broadcaster | FFlop | Conj deriving Eq
type Key = String
type Item = (Node, Key, [Key])

data Pulse = Low | High deriving (Eq, Show)
type State = (Map String Bool, Map String (Map String Pulse))

-- return type of `graphFromEdges`
type Input = (Graph, Vertex -> Item, Key -> Maybe Vertex)


main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = graphFromEdges . map parseLine . lines

parseLine :: String -> Item
parseLine line = (node, key, targets)
  where
    [c:key', targets'] = splitOn " -> " line
    targets = splitOn ", " targets'
    node = case c of
      'b' -> Broadcaster
      '%' -> FFlop
      '&' -> Conj
      _   -> undefined
    key = if c == 'b' then c:key' else key'

part1 :: Input -> Int
-- part1 input = pulseProduct $ push 1000 input $ initialState input
part1 = pulseProduct . push 1000

part2 :: Input -> Int
part2 _ = 0

initialState :: Input -> State
initialState (g, nfv, vfk) = (ffState, cState)
  where
    nodes = map nfv $ vertices g
    ffState = Map.fromList [(k, False) | (n, k, _) <- nodes, n == FFlop]
    cState = Map.fromList
      [ (k, Map.fromList [(k', Low) | k' <- inKs])
      | (n, k, _) <- nodes
      , n == Conj
      , let inKs = [sndOf3 (nfv v) | (v, thisV) <- edges g, Just thisV == vfk k]
      ]

push :: Int -> Input -> [Pulse]
push n inp = concatMap snd . take n . tail $ iterate (push' inp . fst) (initialState inp, [])

push' :: Input -> State -> (State, [Pulse])
-- first pulse from button also counts
push' (_, nfv, vfk) s = go s [] [("", Low, "broadcaster")]
  where
    go state ps [] = (state, ps)
    go state ps ((src, p, key):xs) = case vfk key of
      Nothing -> go state (p:ps) xs
      Just v -> go state' (p:ps) (xs ++ xs')
        where
          (n, _, ks) = nfv v
          (state', p') = step n
          xs' = [(key, fromJust p', k) | isJust p', k <- ks]

          step :: Node -> (State, Maybe Pulse)
          step Broadcaster = (state, Just Low)
          step FFlop
            | p == High = (state, Nothing)
            | otherwise =
              (state & (_1 . at key) %~ fmap not
              , fmap ffToP $ state ^. (_1 . at key)
              )
          step Conj = (state', Just $ foldl mergeP Low $ state' ^. (_2 . ix key))
            where state' = state & (_2 . at key . _Just . at src) ?~ p

          ffToP False = High
          ffToP True = Low
          mergeP Low High = Low
          mergeP _ _ = High

pulseProduct = uncurry (*) . over both length . partition (== Low)
