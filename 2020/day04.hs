import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- map (unwords . lines) . splitOn "\n\n" <$> readFile "input/day04.txt"
  print $ day4 input validateA
  print $ day4 input validateB

day4 :: [String] -> ([[String]] -> Bool) -> Int
day4 input vldt = length $ filter vldt $ map (map (splitOn ":") . words) input

validateA :: [[String]] -> Bool
validateA =
  null . (\\) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] . map head

validateB :: [[String]] -> Bool
validateB input = validateA input && all val input
  where
    val [x, b]
      | "byr" <- x = pars >= 1920 && pars <= 2002
      | "iyr" <- x = pars >= 2010 && pars <= 2020
      | "eyr" <- x = pars >= 2020 && pars <= 2030
      | "hgt" <- x, (y, "cm") <- hgt = read y >= 150 && read y <= 193
      | "hgt" <- x, (y, "in") <- hgt = read y >= 59 && read y <= 76
      | "hcl" <- x =
        (head b == '#') &&
        all (`elem` (['0' .. '9'] `union` ['a' .. 'f'])) (tail b) &&
        (length b == 7)
      | "ecl" <- x = b `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      | "pid" <- x = length b == 9 && null (dropWhile isDigit b)
      | "cid" <- x = True
      | _ <- x = False
      where
        pars = read b
        hgt = partition isDigit b
