module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "L68"
    , "L30"
    , "R48"
    , "L5"
    , "R60"
    , "L55"
    , "L1"
    , "L99"
    , "R14"
    , "L82"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day01.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 10

      it "parses real input (length)" $
        length input `shouldBe` 4543

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 3

      it "computes accepted result" $
        part1 input `shouldBe` 1135

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 6

      it "computes accepted result" $
        part2 input `shouldBe` 6558

      let testP2 inp res = it ("computes expected result for " ++ show inp) $ do
            part2 inp `shouldBe` res

      testP2 [-50] 1
      testP2 [50] 1
      testP2 [-150] 2
      testP2 [-100, -50] 2
      testP2 [150] 2
      testP2 [250] 3
      testP2 [-100] 1
      testP2 [-100, 100] 2
      testP2 [-50, -50, 50, 50] 2
      testP2 [-50, 1] 1
      testP2 [50, -1] 1
