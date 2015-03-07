module Main where

import Prelude hiding ((^))
import Control.Monad.Writer
import Text.Printf (printf)

import Factor
import Bayes

fh  = 0
fs  = 1
fb  = 2
fm  = 3
ndh = 4
na  = 5

term :: BayesTerm -> Writer [BayesTerm] ()
term t = tell [t]

termWithNeg :: BayesTerm -> Writer [BayesTerm] ()
termWithNeg t@(BayesTerm (allAssigs -> assigs) p) = do
  term t
  let (var, val):vs = assigs
  term $ BayesTerm (BayesAssgs $ (var, not val):vs) (1-p)

problem :: [BayesTerm]
problem = snd . runWriter $ do
  termWithNeg $ P(fs)                      .= 0.05
  termWithNeg $ P(fb .|. fs)               .= 0.6
  termWithNeg $ P(fb .|. -fs)              .= 0.1
  termWithNeg $ P(fm)                      .= 0.0357
  termWithNeg $ P(na)                      .= 0.3
  termWithNeg $ P(ndh .|. na ^ fm)         .= 0.8
  termWithNeg $ P(ndh .|. na ^ -fm)        .= 0.5
  termWithNeg $ P(ndh .|. -na ^ fm)        .= 0.4
  termWithNeg $ P(ndh .|. -na ^ -fm)       .= 0
  termWithNeg $ P(fh .|. fs ^ ndh ^ fm)    .= 0.99
  termWithNeg $ P(fh .|. fs ^ ndh ^ -fm)   .= 0.75
  termWithNeg $ P(fh .|. fs ^ -ndh ^ fm)   .= 0.9
  termWithNeg $ P(fh .|. fs ^ -ndh ^ -fm)  .= 0.5
  termWithNeg $ P(fh .|. -fs ^ ndh ^ fm)   .= 0.65
  termWithNeg $ P(fh .|. -fs ^ ndh ^ -fm)  .= 0.2
  termWithNeg $ P(fh .|. -fs ^ -ndh ^ fm)  .= 0.4
  termWithNeg $ P(fh .|. -fs ^ -ndh ^ -fm) .= 0

ndhPrior :: Prob
ndhPrior = compute problem
            (P(ndh))
            [P(na), P(fm), P(ndh .|. fm ^ na)]
            $ map getVar [na, fm]

fhPrior :: Prob
fhPrior = compute problem
            (P(fh))
            [P(na), P(fm), P(ndh .|. fm ^ na), P(fs), P(fh .|. fs ^ ndh ^ fm)]
            $ map getVar [na, fm, ndh, fs]

fsGivenHowlMoon :: Prob
fsGivenHowlMoon = compute problem
                    (P(fs .|. fh ^ fm))
                    [P(fm), P(fs), P(fh .|. fs ^ fm ^ ndh)]
                    $ map getVar [fb, ndh]

fsGivenHowlMoonBowl :: Prob
fsGivenHowlMoonBowl = compute problem
                        (P(fs .|. fh ^ fm ^ fb))
                        [P(fm), P(fs), P(fb .|. fs), P(fh .|. fs ^ fm ^ ndh)]
                        $ map getVar [ndh]

fsGivenHowlMoonBowlAway :: Prob
fsGivenHowlMoonBowlAway = compute problem
                            (P(fs .|. fh ^ fm ^ fb ^ na))
                            [P(na), P(fm), P(fs), P(fb .|. fs), P(ndh .|. fm ^ na), P(fh .|. fs ^ fm ^ ndh)]
                            $ map getVar [ndh]


main :: IO ()
main = do
  let formatDouble = printf "%f"
  putStrLn "\n-------------------------"
  putStrLn   "Bayesian Inference Solver"
  putStrLn   "-------------------------\n"
  putStrLn $ "P(ndh) = " ++ formatDouble ndhPrior
  putStrLn ""
  putStrLn $ "q2: P(fh) = " ++ formatDouble fhPrior
  putStrLn $ "q3: P(fs|fh,fm) = " ++ formatDouble fsGivenHowlMoon
  putStrLn $ "q4: P(fs|fh,fm,fb) = " ++ formatDouble fsGivenHowlMoonBowl
  putStrLn $ "q5: P(fs|fh,fm,fb,na) = " ++ formatDouble fsGivenHowlMoonBowlAway

