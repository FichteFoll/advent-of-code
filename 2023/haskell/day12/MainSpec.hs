module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "???.### 1,1,3"
    , ".??..??...?##. 1,1,3"
    , "?#?#?#?#?#?#?#? 1,3,1,6"
    , "????.#...#... 4,1,1"
    , "????.######..#####. 1,6,5"
    , "?###???????? 3,2,1"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day12.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 6

      it "parses real input (length)" $
        length input `shouldBe` 1000

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 21

      it "computes accepted result" $
        part1 input `shouldBe` 7599

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --      part2 input `shouldBe` 0

    -- describe "countCombinations" $ do
    --   it "counts valid combinations of each the some example input line" $
    --     (uncurry .) countCombinations Nothing (exampleInput !! 1) `shouldBe` 8


    describe "countCombinations" $ do
      it "counts valid combinations of each example input line" $
        map ((uncurry .) countCombinations Nothing) exampleInput `shouldBe` [1, 4, 1, 1, 4, 10]
