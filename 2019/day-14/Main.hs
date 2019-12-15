module Main where

import Data.List (find)
import Data.List.Split (splitOn)
-- import Data.Map (Map, (!), (\\))
import Data.Map hiding (map)
import qualified Data.Map as Map
import Data.Maybe

type Material = String
type Mats = Map Material Int
data Reaction = React {
  components :: Mats,
  result :: Material,
  quantity :: Int
  } deriving (Show)
type Nanofactory = Map Material Reaction

data MatView = MV {storage :: Mats, needed :: Mats } deriving (Show)


parse :: String -> Nanofactory
parse = fromList . map (\r -> (result r, r)) . map parseReaction . lines
  where
    parseReaction line = React (fromList $ map parseComponent $ splitOn ", " src) res q
                         where
                          (src:dst:[]) = splitOn " => " line
                          (res, q) = parseComponent dst
    parseComponent comp = let (num:name:[]) = splitOn " " comp in (name, read num)


resolveMats :: Nanofactory -> MatView -> MatView
resolveMats nf mv
  | Just mat <- find (`member` nf) $ keys $ needed mv =
    let
      -- Fetch recipe and required applications
      amount = needed mv ! mat
      recipe = nf ! mat
      (q, r) = amount `quotRem` quantity recipe
      times = q + signum r
      matsFromRecipe = Map.map (* times) (components recipe)
      -- Move newly produced mat from needed to storage
      neededNoMat = delete mat $ needed mv
      addToStorage = times * quantity recipe - amount
      storageMat = alter (addMaybe addToStorage) mat $ storage mv
      -- Add new required mats to needed; utilizing leftovers from storage
      addToNeeded = unionWith saturatingSub matsFromRecipe (intersection storageMat matsFromRecipe)
      takenFromStorage = intersectionWith (-) matsFromRecipe addToNeeded
      newStorage = Map.filter (/= 0) $ unionWith (-) storageMat takenFromStorage
      newNeeded = unionWith (+) neededNoMat addToNeeded
      newMv = MV newStorage newNeeded
    in resolveMats nf newMv
  | otherwise = mv

addMaybe :: (Num a, Eq a) => a -> Maybe a -> Maybe a
addMaybe n mm | Just m <- mm, m + n /= 0 = Just (m + n)
              | Nothing <- mm, n /= 0 = Just n
              | otherwise = Nothing

saturatingSub :: (Num a, Ord a) => a -> a -> a
saturatingSub a b = if b > a then 0 else a - b

makeFuel :: Nanofactory -> MatView -> Int -> Int
makeFuel nf mv i | Just _ <- Map.lookup "ORE" (storage resolvedMv) = makeFuel nf newMv (i+addCounter)
                 | otherwise = i - fuelCreated
  where
    resolvedMv = resolveMats nf mv
    oreUsed = (storage mv ! "ORE") - (storage resolvedMv ! "ORE")
    fuelCreated = (needed mv ! "FUEL")
    iterationsRemaining = (storage resolvedMv ! "ORE") `div` (oreUsed `div` fuelCreated)
    addCounter = max 1 (iterationsRemaining `div` 2)
    newMv = resolvedMv { needed = insert "FUEL" addCounter (needed resolvedMv) }

part1 :: Nanofactory -> Int
part1 = flip (!) "ORE" . needed . flip resolveMats (MV empty $ singleton "FUEL" 1)

part2 :: Nanofactory -> Int
part2 nf = makeFuel nf (MV (singleton "ORE" 1000000000000) (singleton "FUEL" 1)) 1

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
