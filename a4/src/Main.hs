module Main where

import Prelude hiding ((^))
import Control.Monad.Writer
import Control.Monad.Reader
import Text.Printf (printf)

import Factor
import Bayes

fh  = 0
fs  = 1
fb  = 2
fm  = 3
ndh = 4
na  = 5

varName :: Var -> String
varName 0 = "fh"
varName 1 = "fs"
varName 2 = "fb"
varName 3 = "fm"
varName 4 = "ndh"
varName 5 = "na"
varName _ = undefined

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

ndhPrior :: (Prob, FactorLog)
ndhPrior = compute problem
            (P(ndh))
            [P(na), P(fm), P(ndh .|. fm ^ na)]
            $ map getVar [na, fm]

fhPrior :: (Prob, FactorLog)
fhPrior = compute problem
            (P(fh))
            [P(na), P(fm), P(ndh .|. fm ^ na), P(fs), P(fh .|. fs ^ ndh ^ fm)]
            $ map getVar [na, fm, ndh, fs]

fsGivenHowlMoon :: (Prob, FactorLog)
fsGivenHowlMoon = compute problem
                    (P(fs .|. fh ^ fm))
                    [P(fm), P(fs), P(fh .|. fs ^ fm ^ ndh)]
                    $ map getVar [fb, ndh]

fsGivenHowlMoonBowl :: (Prob, FactorLog)
fsGivenHowlMoonBowl = compute problem
                        (P(fs .|. fh ^ fm ^ fb))
                        [P(fm), P(fs), P(fb .|. fs), P(fh .|. fs ^ fm ^ ndh)]
                        $ map getVar [ndh]

fsGivenHowlMoonBowlAway :: (Prob, FactorLog)
fsGivenHowlMoonBowlAway = compute problem
                            (P(fs .|. fh ^ fm ^ fb ^ na))
                            [P(na), P(fm), P(fs), P(fb .|. fs), P(ndh .|. fm ^ na), P(fh .|. fs ^ fm ^ ndh)]
                            $ map getVar [ndh]

render :: (Prob, FactorLog) -> String -> IO ()
render (p, logs) prob_str = do
  putStrLn $ "------------------ " ++ prob_str ++ " ----------------------------------"
  mapM_ putStrLn $ map (flip runReader varName) logs
  putStrLn "ANSWER:"
  putStrLn $ prob_str ++ " = " ++ printf "%f" p
  putStr "\n\n"

main :: IO ()
main = do
  putStrLn "\n-------------------------"
  putStrLn   "Bayesian Inference Solver"
  putStrLn   "-------------------------\n"
  --render ndhPrior "P(ndh)"
  render fhPrior "q2: P(fh)"
  render fsGivenHowlMoon "q3: P(fs|fh,fm)"
  render fsGivenHowlMoonBowl "q4: P(fs|fh,fm,fb)"
  render fsGivenHowlMoonBowlAway "q5: P(fs|fh,fm,fb,na)"

