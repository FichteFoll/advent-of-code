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
        example2 = parse $ concat ["########################\n"
                                  ,"#f.D.E.e.C.b.A.@.a.B.c.#\n"
                                  ,"######################.#\n"
                                  ,"#d.....................#\n"
                                  ,"########################\n"]
        example3 = parse $ concat ["########################\n"
                                  ,"#...............b.C.D.f#\n"
                                  ,"#.######################\n"
                                  ,"#.....@.a.B.c.d.A.e.F.g#\n"
                                  ,"########################\n"]
        example4 = parse $ concat ["#################\n"
                                  ,"#i.G..c...e..H.p#\n"
                                  ,"########.########\n"
                                  ,"#j.A..b...f..D.o#\n"
                                  ,"########@########\n"
                                  ,"#k.E..a...g..B.n#\n"
                                  ,"########.########\n"
                                  ,"#l.F..d...h..C.m#\n"
                                  ,"#################\n"]
        example5 = parse $ concat ["########################\n"
                                  ,"#@..............ac.GI.b#\n"
                                  ,"###d#e#f################\n"
                                  ,"###A#B#C################\n"
                                  ,"###g#h#i################\n"
                                  ,"########################\n"]

    describe "part1" $ do
      it "collects keys for example 1" $
        part1 example1 `shouldBe` 8

      it "collects keys for example 2" $
        part1 example2 `shouldBe` 86

      it "collects keys for example 3" $
        part1 example3 `shouldBe` 132

      -- Takes 60 seconds (17 if compiled)
      -- it "collects keys for example 4" $
      --   part1 example4 `shouldBe` 136

      it "collects keys for example 5" $
        part1 example5 `shouldBe` 81

    -- describe "part1" $ do
    --   it "computes accepted result" $
    --     part1 input `shouldBe` 5182 -- 5212

    -- describe "part2" $ do
    --   it "computes accepted result" $
    --     part2 input `shouldBe` 929045
