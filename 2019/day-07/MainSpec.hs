module MainSpec where

import Test.Hspec
import Test.QuickCheck
import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "run" $ do
      let run' = snd . fst . flip run []
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

      context "when provided with input" $ do
        let runInput n xs = head $ snd $ snd $ run xs [n]
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

    describe "runSeq & part1" $ do
      context "for example 1" $ do
        let code = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
            seq = [4,3,2,1,0]
            result = 43210
        it "runSeq runs sequence" $
          runSeq code 0 seq `shouldBe` result
        it "part1 finds correct result" $
          part1 code `shouldBe` result

      context "for example 2" $ do
        let code = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
            seq = [0,1,2,3,4]
            result = 54321
        it "runSeq runs sequence" $
          runSeq code 0 seq `shouldBe` result
        it "part1 finds correct result" $
          part1 code `shouldBe` result

      context "for example 3" $ do
        let code = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
            seq = [1,0,4,3,2]
            result = 65210
        it "runSeq runs sequence" $
          runSeq code 0 seq `shouldBe` result
        it "part1 finds correct result" $
          part1 code `shouldBe` result
    
    describe "part1" $ do
      it "computes accepted result" $
         part1 input `shouldBe` 75228

    -- describe "part2" $ do
    --   it "computes accepted result" $
    --      part2 input `shouldBe` 9265694
