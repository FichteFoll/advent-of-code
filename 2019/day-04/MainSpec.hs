module MainSpec where

import Test.Hspec
import qualified Data.Set as Set

import Main hiding (main)


main :: IO ()
main = do
  let input = [264793..803935]
  hspec $ do
    describe "isPass" $ do
      it "accepts all 1" $
        isPass (show 111111) `shouldSatisfy` id
      it "fails non-steady" $
        isPass (show 223450) `shouldSatisfy` not
      it "fails no double" $
        isPass (show 123789) `shouldSatisfy` not

    describe "isPass2" $ do
      it "accepts three pairs" $
        isPass2 (show 112233) `shouldSatisfy` id
      it "fails single triple" $
        isPass2 (show 123444) `shouldSatisfy` not
      it "accepts 111122" $
        isPass2 (show 111122) `shouldSatisfy` id

    describe "part1" $ do
      it "computes accepted result" $
         part1 input `shouldBe` 966

    describe "part2" $ do
      it "computes accepted result" $
         part2 input `shouldBe` 628
