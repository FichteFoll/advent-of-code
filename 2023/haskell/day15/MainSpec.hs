module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

-- trailing newline to test that it's stripped
exampleText = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day15.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 11

      it "parses real input (length)" $
        length input `shouldBe` 4000

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 1320

      it "computes accepted result" $
        part1 input `shouldBe` 511215

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --     part2 input `shouldBe` 0

    describe "hash" $ do
      it "computes expected result for 'HASH'" $
        hash "HASH" `shouldBe` 52


    describe "hash" $ do
      it "computes expected results for example input" $
        map hash exampleInput `shouldBe` [30, 253, 97, 47, 14, 180, 9, 197, 48, 214, 231]
