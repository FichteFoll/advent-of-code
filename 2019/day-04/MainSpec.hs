module MainSpec where

import Test.Hspec
import qualified Data.Set as Set

import Main hiding (main)


main :: IO ()
main = do
  let instr = "264793-803935"
      input = parse instr
  hspec $ do
    describe "parse" $ do
      it "parses input" $
        parse "264793-803935" `shouldBe` [264793..803935]

    describe "isPass" $ do
      it "accepts all 1" $
        isPass 111111 `shouldSatisfy` id
      it "fails non-steady" $
        isPass 223450 `shouldSatisfy` not
      it "fails no double" $
        isPass 123789 `shouldSatisfy` not

    describe "isPass2" $ do
      it "accepts three pairs" $
        isPass2 112233 `shouldSatisfy` id
      it "fails single triple" $
        isPass2 123444 `shouldSatisfy` not
      it "accepts 111122" $
        isPass2 111122 `shouldSatisfy` id

    describe "part1" $ do
      it "computes accepted result" $
         part1 input `shouldBe` 966

    describe "part2" $ do
      it "computes accepted result" $
         part2 input `shouldBe` 628
