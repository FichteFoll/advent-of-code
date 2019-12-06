module MainSpec where

import Data.Tree
import Test.Hspec
import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    let example = parse "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
    describe "buildTree" $ do
      it "builds correct tree" $
         buildTree example `shouldBe` Node "COM" [Node "B" [Node "C" [Node "D" [Node "E" [Node "F" [],Node "J" [Node "K" [Node "L" []]]],Node "I" []]],Node "G" [Node "H" []]]]

    describe "part1" $ do
      it "computes example result" $
         part1 example `shouldBe` 42

      it "computes accepted result" $
         part1 input `shouldBe` 200001

    let example2 = parse "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
    describe "part2" $ do
      it "computes example result" $
         part2 example2 `shouldBe` 4

      it "computes accepted result" $
         part2 input `shouldBe` 379
