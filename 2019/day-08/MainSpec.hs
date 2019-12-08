module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)
import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "layerize" $ do
      it "builds 2 layers" $
         layerize (3,2) "123456789012" `shouldBe` ["123456","789012"]

    describe "part1" $ do
      it "computes accepted result" $
         part1 input `shouldBe` 2500

    describe "delayer" $ do
      it "computes example" $
         (delayer $ layerize (2,2) "0222112222120000") `shouldBe` "0110"


    describe "delayer" $ do
      it "computes example" $
         (showImg 25 $ part2 input) `shouldBe` concat
            [" ██  █   ██  █  ██  █  █ \n"
            ,"█  █ █   ██  █ █  █ █  █ \n"
            ,"█     █ █ █  █ █  █ ████ \n"
            ,"█      █  █  █ ████ █  █ \n"
            ,"█  █   █  █  █ █  █ █  █ \n"
            ," ██    █   ██  █  █ █  █ \n"]
