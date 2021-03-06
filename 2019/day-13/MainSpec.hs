module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)

import Main hiding (main)
import Intcode (parse)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "part1" $ do
        it "computes accepted result" $
          part1 input `shouldBe` 247

    describe "part2" $ do
        it "computes accepted result" $
          part2 input `shouldBe` 12954
