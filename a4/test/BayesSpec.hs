{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module BayesSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Array (Array)
import qualified Data.Array as Array
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM)
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
          context "restricts variables" $ do
            it "Eliminates last var" $ restrict (testData 2) 1 False `shouldBe` Factor [0] (Array.array (makeValRange 1) [([False], 0), ([True], 2)])
            it "Prop" $ property $ \(a::Int) -> True

main :: IO ()
main = hspec spec


