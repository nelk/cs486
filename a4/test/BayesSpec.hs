{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module BayesSpec where

import Prelude hiding ((^))
import Test.Hspec

import qualified Data.Array as Array
import Factor
import Bayes

epsilon :: Prob
epsilon = 0.00001

infixr 5 `shouldBeClose`
shouldBeClose :: Prob -> Prob -> Expectation
shouldBeClose actual expected =
  if actual <= expected + epsilon && actual >= expected - epsilon
    then return ()
    else expectationFailure $ "expected: " ++ show expected ++ "\nbut got: " ++ show actual

-- Note: All vars and associated vals have to be in ascending order!
prob :: [Var] -> [([Val], Prob)] -> Factor Prob
prob vars assocs = Factor vars $ Array.array (makeValRange $ length vars) assocs

spec :: Spec
spec = describe "Bayes Variable Elimination" $ do
          context "Restricts variables" $ do
            it "Eliminates a var" $
              let f1 :: Factor Int = Factor [0, 1] $
                    Array.array (makeValRange 2) [ ([False, False], 2)
                                                 , ([False, True ], 3)
                                                 , ([True , False], 5)
                                                 , ([True , True ], 7)
                                                 ]
                  f_restricted = restrict f1 1 False
                  f_expected = Factor [0] (Array.array (makeValRange 1) [([False], 2), ([True], 5)])
              in f_restricted `shouldBe` f_expected

          context "Multiplies variables" $ do
            it "Can multiply unrelated variables" $
              let f1 :: Factor Int = Factor [0] $ Array.array (makeValRange 1) [([False], 2), ([True], 3)]
                  f2 = Factor [1] $ Array.array (makeValRange 1) [([False], 5), ([True], 7)]
                  f_mult = multiply f1 f2
                  f_expected = Factor [0, 1] $ Array.array (makeValRange 2) [([False, False], 10), ([False, True], 14), ([True, False], 15), ([True, True], 21)]
              in f_mult `shouldBe` f_expected

            it "Can multiply same variable" $
              let f1 :: Factor Int = Factor [0] $ Array.array (makeValRange 1) [([False], 2), ([True], 3)]
                  f2 = Factor [0] $ Array.array (makeValRange 1) [([False], 5), ([True], 7)]
                  f_mult = multiply f1 f2
                  f_expected = Factor [0] $ Array.array (makeValRange 1) [([False], 10), ([True], 21)]
              in f_mult `shouldBe` f_expected

            it "Correctly multiples one different and one same" $
              let f1 :: Factor Int = Factor [0, 1] $
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

          context "Sums out variables" $ do
            it "Can sum out a variable from two" $
              let f1 :: Factor Int = Factor [0, 1] $
                    Array.array (makeValRange 2) [ ([False, False], 2)
                                                 , ([False, True ], 3)
                                                 , ([True , False], 5)
                                                 , ([True , True ], 7)
                                                 ]
                  f_summed = sumout f1 1
                  f_expected = Factor [0] $ Array.array (makeValRange 1) [([False], 2+3), ([True], 5+7)]
              in f_summed `shouldBe` f_expected

            it "Can sum out the other variable from two" $
              let f1 :: Factor Int = Factor [0, 1] $
                    Array.array (makeValRange 2) [ ([False, False], 2)
                                                 , ([False, True ], 3)
                                                 , ([True , False], 5)
                                                 , ([True , True ], 7)
                                                 ]
                  f_summed = sumout f1 0
                  f_expected = Factor [1] $ Array.array (makeValRange 1) [([False], 2+5), ([True], 3+7)]
              in f_summed `shouldBe` f_expected

          context "Solves bayes inferences" $ do
            it "Can solve A -> B network. P(a|b) = P(b|a)P(a)" $
              let a = 0
                  b = 1
                  p_a = prob [a] $
                    [ ([False], 0.3)
                    , ([True ], 0.7)
                    ]
                  p_b_given_a = prob [a, b] $
                    [ ([False, False], 0.1)
                    , ([False, True ], 0.2)
                    , ([True , False], 0.6)
                    , ([True , True ], 0.1)
                    ]
                  (p, _) = inference [p_a, p_b_given_a] [(a, True)] [] [(b, True)]
              in p `shouldBeClose` 0.1*0.7/(0.2*0.3+0.1*0.7)

            it "Can solve A -> B <- C. P(A=a|B=b,C=c) = alpha*P(B=b|A=a,C=c)P(A=a)P(C=c)" $
              let a = 0
                  b = 1
                  c = 2
                  p_a = prob [a] $
                    [ ([False], 0.3)
                    , ([True ], 0.7)
                    ]
                  p_c = prob [c] $
                    [ ([False], 0.4)
                    , ([True ], 0.6)
                    ]
                  p_b_given_a_c = prob [a, b, c] $
                    [ ([False, False, False], 0.1)
                    , ([False, False, True ], 0.2)
                    , ([False, True , False], 0.3)
                    , ([False, True , True ], 0.1)
                    , ([True , False, False], 0.1)
                    , ([True , False, True ], 0.1)
                    , ([True , True , False], 0.05)
                    , ([True , True , True ], 0.05)
                    ]
                  (p, _) = inference [p_c, p_a, p_b_given_a_c] [(a, True)] [] [(b, True), (c, True)]
              in p `shouldBeClose` 0.7*0.05/(0.7*0.05 + 0.3*0.1)

            it "DSL: Can solve A -> B <- C. P(A=a|B=b,C=c) = alpha*P(B=b|A=a,C=c)P(A=a)P(C=c)" $
              let a = 0
                  b = 1
                  c = 2
                  (p, _) = compute [ P(-a) .= 0.3
                                   , P( a) .= 0.7
                                   , P(-c) .= 0.4
                                   , P( c) .= 0.6
                                   , P(-b .|. -a ^ -c) .= 0.1
                                   , P(-b .|. -a ^  c) .= 0.2
                                   , P(-b .|.  a ^ -c) .= 0.1
                                   , P(-b .|.  a ^  c) .= 0.1
                                   , P( b .|. -a ^ -c) .= 0.3
                                   , P( b .|. -a ^  c) .= 0.1
                                   , P( b .|.  a ^ -c) .= 0.05
                                   , P( b .|.  a ^  c) .= 0.05
                         ] (P(a .|. b ^ c)) [P(c), P(a), P(b .|. a ^ c)] []
              in p `shouldBeClose` 0.7*0.05/(0.7*0.05 + 0.3*0.1)

            it "DSL: Notes ex. A -> B -> C. P(c) = P(c|b)P(b|a)P(a)" $
              let a = 0
                  b = 1
                  c = 2
                  (p, _) = compute [ P(-a) .= 0.1
                                   , P( a) .= 0.9
                                   , P(-b .|. -a) .= 0.6
                                   , P(-b .|.  a) .= 0.1
                                   , P( b .|. -a) .= 0.4
                                   , P( b .|.  a) .= 0.9
                                   , P(-c .|. -b) .= 0.8
                                   , P(-c .|.  b) .= 0.3
                                   , P( c .|. -b) .= 0.2
                                   , P( c .|.  b) .= 0.7
                        ] (P(c)) [P(a), P(b .|. a), P(c .|. b)] []
              in p `shouldBeClose` 0.625

main :: IO ()
main = hspec spec


