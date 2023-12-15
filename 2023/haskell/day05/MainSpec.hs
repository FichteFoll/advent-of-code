module MainSpec (main) where

import Control.Lens
import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "seeds: 79 14 55 13"
    , ""
    , "seed-to-soil map:"
    , "50 98 2"
    , "52 50 48"
    , ""
    , "soil-to-fertilizer map:"
    , "0 15 37"
    , "37 52 2"
    , "39 0 15"
    , ""
    , "fertilizer-to-water map:"
    , "49 53 8"
    , "0 11 42"
    , "42 0 7"
    , "57 7 4"
    , ""
    , "water-to-light map:"
    , "88 18 7"
    , "18 25 70"
    , ""
    , "light-to-temperature map:"
    , "45 77 23"
    , "81 45 19"
    , "68 64 13"
    , ""
    , "temperature-to-humidity map:"
    , "0 69 1"
    , "1 0 69"
    , ""
    , "humidity-to-location map:"
    , "60 56 37"
    , "56 93 4"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day05.txt"
  let exampleInput = parse exampleText
  let exampleMaps = snd exampleInput

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        bimap length length exampleInput `shouldBe` (4, 7)

      it "parses real input (length)" $
        bimap length length input `shouldBe` (20, 7)

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 35

      it "computes accepted result" $
        part1 input `shouldBe` 457535844

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 46

      it "computes accepted result" $
        part2 input `shouldBe` 41222968

    describe "resolve, on the example input," $ do
      it "resolves seed 79 of the example input" $
        scanl resolve 79 exampleMaps `shouldBe` [79, 81, 81, 81, 74, 78, 78, 82]

      it "resolves seed 82 of the example input" $
        scanl resolve 82 exampleMaps `shouldBe` [82, 84, 84, 84, 77, 45, 46, 46]

    describe "resolveRange, on the first example input map," $ do
      let m = head exampleMaps
      it "maps '79 1' to a single range" $
        resolveRange [(79, 79)] m `shouldBe` [(81, 81)]

      it "maps '97 4' to three ranges (map 2, map 1, id)" $
        resolveRange [(97, 100)] m `shouldBe` [(99, 99), (50, 51), (100, 100)]

      it "maps '49 2' to two ranges (id, map 2)" $
        resolveRange [(49, 50)] m `shouldBe` [(49, 49), (52, 52)]

      it "maps '0 100' to four ranges (id, map 2, map 1, id)" $
        resolveRange [(0, 100)] m `shouldBe` [(0, 49), (52, 99), (50, 51), (100, 100)]
