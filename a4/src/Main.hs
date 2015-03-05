{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Set as Set

import Factor
import Bayes

main :: IO ()
main = let a :: Array [Val] Prob = Array.array ([False, False, False], [True, True, True])
                                    [([x, y, z], 2*(fromIntegral(fromEnum x)*2 + fromIntegral(fromEnum y)) + fromIntegral(fromEnum z)) | x <- allVals, y <- allVals, z <- allVals]
       in print $ restrict (Factor [0, 1, 2] a) 2 False

