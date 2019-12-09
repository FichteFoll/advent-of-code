module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)
import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    let runAmp tape inp = run (newAmp tape []) inp
    describe "run" $ do
      let run' tape = newCode where ((_, newCode), _) = runAmp tape []
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
        let runInput n tape = head $ snd $ snd $ runAmp tape [n]
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

    describe "computeSeqLoop & part1" $ do
      context "for example 1" $ do
        let tape = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
            seq = [4,3,2,1,0]
            result = 43210
        it "computeSeqLoop runs sequence" $
          computeSeqLoop tape seq `shouldBe` result
        it "part1 finds correct result" $
          part1 tape `shouldBe` result

      context "for example 2" $ do
        let tape = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
            seq = [0,1,2,3,4]
            result = 54321
        it "computeSeqLoop runs sequence" $
          computeSeqLoop tape seq `shouldBe` result
        it "part1 finds correct result" $
          part1 tape `shouldBe` result

      context "for example 3" $ do
        let tape = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
            seq = [1,0,4,3,2]
            result = 65210
        it "computeSeqLoop runs sequence" $
          computeSeqLoop tape seq `shouldBe` result
        it "part1 finds correct result" $
          part1 tape `shouldBe` result
    
    describe "part1" $ do
      it "computes accepted result" $
         part1 input `shouldBe` 75228

    describe "computeSeqLoop & part2" $ do
      context "for example 1" $ do
        let tape = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
            seq = [9,8,7,6,5]
            result = 139629729
        it "computeSeqLoop multi-runs sequence" $
          computeSeqLoop tape seq `shouldBe` result
        it "part2 finds correct result" $
          part2 tape `shouldBe` result

      context "for example 2" $ do
        let tape = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,
                    1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,
                    56,6,99,0,0,0,0,10]
            seq = [9,7,8,5,6]
            result = 18216
        it "computeSeqLoop multi-runs sequence" $
          computeSeqLoop tape seq `shouldBe` result
        it "part2 finds correct result" $
          part2 tape `shouldBe` result

    describe "part2" $ do
      it "computes accepted result" $
         part2 input `shouldBe` 79846026
