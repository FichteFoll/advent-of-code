module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)

import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "fft" $ do
        it "first iteration" $
          fft (parse "12345678") `shouldBe` [4,8,2,2,6,1,5,8]
        it "second iteration" $
          fft [4,8,2,2,6,1,5,8] `shouldBe` [3,4,0,4,0,4,3,8]
        it "third iteration" $
          fft [3,4,0,4,0,4,3,8] `shouldBe` [0,3,4,1,5,5,1,8]
        it "fourth iteration" $
          fft [0,3,4,1,5,5,1,8] `shouldBe` [0,1,0,2,9,4,9,8]

    describe "part1" $ do
        it "computes example 1" $
          part1 (parse "80871224585914546619083218645595") `shouldBe` "24176176"
        it "computes example 2" $
          part1 (parse "19617804207202209144916044189917") `shouldBe` "73745418"
        it "computes example 3" $
          part1 (parse "69317163492948606335995924319873") `shouldBe` "52432133"

        -- it "computes accepted result" $
        --   part1 input `shouldBe` "37153056"

    describe "part2" $ do
        it "computes example 1" $
          part2 (parse "03036732577212944063491565474664") `shouldBe` "84462026"
        it "computes example 2" $
          part2 (parse "02935109699940807407585447034323") `shouldBe` "78725270"
        it "computes example 3" $
          part2 (parse "03081770884921959731165446850517") `shouldBe` "53553731"

        -- it "computes accepted result" $
        --   part2 input `shouldBe` "60592199"
