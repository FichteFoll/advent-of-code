module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)
import qualified Data.Map as Map
import Data.Map ((!))

import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    let example1 = parse "#########\n#b.A.@.a#\n#########"
        findKey m key = shortestPath m (dkeys m ! key)
        example2 = parse $ concat ["########################\n"
                                  ,"#f.D.E.e.C.b.A.@.a.B.c.#\n"
                                  ,"######################.#\n"
                                  ,"#d.....................#\n"
                                  ,"########################\n"]
        example5 = parse $ concat ["########################\n"
                                  ,"#@..............ac.GI.b#\n"
                                  ,"###d#e#f################\n"
                                  ,"###A#B#C################\n"
                                  ,"###g#h#i################\n"
                                  ,"########################\n"]

    describe "shortestPath" $
      context "for example 1" $ do
        it "finds distance to 'a'" $
          findKey example1 'a' `shouldBe` Just 2
        it "does not find distance to 'b'" $
          findKey example1 'b' `shouldBe` Nothing
        it "finds distance to 'b' without 'A'" $
          findKey (example1 {doors = Map.empty}) 'b' `shouldBe` Just 4

    describe "collectKeys" $ do
      it "collects keys for example 1" $
        collectKeys example1 `shouldBe` 8

      it "collects keys for example 2" $
        collectKeys example2 `shouldBe` 86

      it "collects keys for example 5" $
        collectKeys example5 `shouldBe` 81

    -- describe "part1" $ do
    --   it "computes accepted result" $
    --     part1 input `shouldBe` 7404

    -- describe "part2" $ do
    --   it "computes accepted result" $
    --     part2 input `shouldBe` 929045
