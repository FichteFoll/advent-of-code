module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "7,1"
    , "11,1"
    , "11,7"
    , "9,7"
    , "9,5"
    , "2,5"
    , "2,3"
    , "7,3"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day09.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 8

      it "parses real input (length)" $
        length input `shouldBe` 496

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 50

      it "computes accepted result" $
        part1 input `shouldBe` 4748985168

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --     part2 input `shouldBe` 0
