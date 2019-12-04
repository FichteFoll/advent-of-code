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
      it "passes all 1" $
        isPass 111111 `shouldSatisfy` id
      it "fails non-steady" $
        isPass 223450 `shouldSatisfy` not
      it "fails no double" $
        isPass 123789 `shouldSatisfy` not
