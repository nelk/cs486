{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module BayesSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Array (Array)
import qualified Data.Array as Array
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM)
import Factor
import Bayes

import Debug.Trace (trace)

simple :: Expectation
simple = length [2] `shouldBe` 1

testData :: Int -> Factor Int Unnormalized
testData l = let keys = replicateM l allVals
                 assocs = zip keys [0..]
                 arr = Array.array (makeValRange l) assocs
             in Factor [0..l-1] arr

spec :: Spec
spec = describe "Bayes Variable Elimination" $ do
          context "Restricts variables" $ do
            it "Eliminates last var" $ restrict (testData 2) 1 False `shouldBe` Factor [0] (Array.array (makeValRange 1) [([False], 0), ([True], 2)])
            --it "Prop" $ property $ \(a::Int) -> True
          context "Multiplies variables" $ do
            it "Can multiply unrelated variables" $
              let f1 :: Factor Int Unnormalized = Factor [0] $ Array.array (makeValRange 1) [([False], 2), ([True], 3)]
                  f2 = Factor [1] $ Array.array (makeValRange 1) [([False], 5), ([True], 7)]
                  f_mult = multiply f1 f2
                  f_expected = Factor [0, 1] $ Array.array (makeValRange 2) [([False, False], 10), ([False, True], 14), ([True, False], 15), ([True, True], 21)]
              in f_mult `shouldBe` f_expected

            it "Can multiply same variable" $
              let f1 :: Factor Int Unnormalized = Factor [0] $ Array.array (makeValRange 1) [([False], 2), ([True], 3)]
                  f2 = Factor [0] $ Array.array (makeValRange 1) [([False], 5), ([True], 7)]
                  f_mult = multiply f1 f2
                  f_expected = Factor [0] $ Array.array (makeValRange 1) [([False], 10), ([True], 21)]
              in f_mult `shouldBe` f_expected

            it "Correctly multiples one different and one same" $
              let f1 :: Factor Int Unnormalized = Factor [0, 1] $
                    Array.array (makeValRange 2) [ ([False, False], 2)
                                                 , ([False, True ], 3)
                                                 , ([True , False], 5)
                                                 , ([True , True ], 7)
                                                 ]
                  f2 = Factor [1] $
                    Array.array (makeValRange 1) [([False], 11), ([True], 13)]
                  f_mult = multiply f1 f2
                  f_expected = Factor [0, 1] $ Array.array (makeValRange 2)
                    [ ([False, False], 2*11)
                    , ([False, True ], 3*13)
                    , ([True , False], 5*11)
                    , ([True , True ], 7*13)
                    ]
              in f_mult `shouldBe` f_expected


main :: IO ()
main = hspec spec


