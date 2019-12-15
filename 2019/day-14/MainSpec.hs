module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)
import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "part1" $ do
        it "computes example 1" $
          let example = parse "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL" in
          part1 example `shouldBe` 31

        it "computes example 2" $
          let example = parse "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL" in
          part1 example `shouldBe` 165

        it "computes accepted result" $
          part1 input `shouldBe` 443537

    -- describe "part2" $ do
    --     it "computes accepted result" $
    --       part1 input `shouldBe` 12954
