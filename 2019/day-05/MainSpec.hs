module MainSpec where

import Test.Hspec
import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "run" $ do
      let run' = fst . flip (run 0) []
      it "computes simple example 1" $
         run' [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
      it "computes simple example 2" $
         run' [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
      it "computes simple example 3" $
         run' [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      it "computes simple example 4" $
         run' [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
      it "computes complex example" $
         run' [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]

      it "computes code 3 with value mode" $
        run' [1002,4,3,4,33] `shouldBe` [1002,4,3,4,99]
      it "handles negative values" $
        run' [1101,100,-1,4,0] `shouldBe` [1101,100,-1,4,99]

    -- describe "part1" $ do
    --   it "computes accepted result" $
    --      part1 input `shouldBe` 4576384

    -- describe "part2" $ do
    --   it "computes accepted result" $
    --      part2 input `shouldBe` 5398
