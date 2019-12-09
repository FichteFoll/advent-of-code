module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)
import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    let runAmp tape inp = run (newAmp {tape = tape}) inp
    describe "run" $ do
      let run' intape = tape $ run (newAmp {tape = intape}) []
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

      it "computes tape 3 with value mode" $
        run' [1002,4,3,4,33] `shouldBe` [1002,4,3,4,99]
      it "handles negative values" $
        run' [1101,100,-1,4,0] `shouldBe` [1101,100,-1,4,99]

      context "when provided with input" $ do
        let runInput n intape = head $ output $ run (newAmp {tape = intape}) [n]
        it "calculates ((==) 8) (pos mode)" $ property $
          \n -> runInput n [3,9,8,9,10,9,4,9,99,-1,8] == fromEnum (n == 8)
        it "calculates ((<) 8) (pos mode)" $ property $
          \n -> runInput n [3,9,7,9,10,9,4,9,99,-1,8] == fromEnum (n < 8)
        it "calculates ((==) 8) (imm mode)" $ property $
          \n -> runInput n [3,3,1108,-1,8,3,4,3,99] == fromEnum (n == 8)
        it "calculates ((<) 8) (imm mode)" $ property $
          \n -> runInput n [3,3,1107,-1,8,3,4,3,99] == fromEnum (n < 8)
        it "calculates ((/=) 0) (pos mode)" $ property $
          \n -> runInput n [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] == fromEnum (n /= 0)
        it "calculates ((/=) 0) (imm mode)" $ property $
          \n -> runInput n [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] == fromEnum (n /= 0)

        it "runs example program" $ property $
          let program = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,
                         1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,
                         1105,1,46,98,99] in
          \n -> runInput n program == 1000 + fromEnum (n > 8) - fromEnum (n < 8)

      context "with the examples of day 9" $ do
        let runOutput intape = output $ run (newAmp {tape = intape}) []
        it "produces a copy of itself" $
          let intape = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] in
          runOutput intape `shouldBe` intape

        it "outputs large number" $
          runOutput [104,1125899906842624,99] `shouldBe` [1125899906842624]

    describe "part1" $ do
        it "computes accepted result" $
          part1 input `shouldBe` [3335138414]

    -- Takes too long to compute in a test
    -- describe "part2" $ do
    --     it "computes accepted result" $
    --       part1 input `shouldBe` [49122]

