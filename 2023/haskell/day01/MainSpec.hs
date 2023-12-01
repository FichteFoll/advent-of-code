module MainSpec where

import Test.Hspec
import Main hiding (main)

exampleInput
  = [ "1abc2"
    , "pqr3stu8vwx"
    , "a1b2c3d4e5f"
    , "treb7uchet"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day01.txt"

  hspec $ do
    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 142

      it "computes accepted result" $
        part1 input `shouldBe` 54951

  --   describe "part2" $ do
  --     it "computes accepted result" $
  --        part2 input `shouldBe` 5398
