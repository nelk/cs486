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
            [P(fh .|. fs ^ ndh ^ fm), P(fm), P(fs), P(ndh .|. fm ^ na), P(na)]
            $ map getVar [na, fm, ndh, fs]

fsGivenHowlMoon :: (Prob, FactorLog)
fsGivenHowlMoon = compute problem
                    (P(fs .|. fh ^ fm))
                    [P(fh .|. fs ^ fm ^ ndh), P(fs), P(fm)]
                    $ map getVar [fb, ndh]

fsGivenHowlMoonBowl :: (Prob, FactorLog)
fsGivenHowlMoonBowl = compute problem
                        (P(fs .|. fh ^ fm ^ fb))
                        [P(fh .|. fs ^ fm ^ ndh), P(fb .|. fs), P(fs), P(fm)]
                        $ map getVar [ndh]

fsGivenHowlMoonBowlAway :: (Prob, FactorLog)
fsGivenHowlMoonBowlAway = compute problem
                            (P(fs .|. fh ^ fm ^ fb ^ na))
                            [P(fh .|. fs ^ fm ^ ndh), P(ndh .|. fm ^ na), P(fb .|. fs), P(fs), P(fm), P(na)]
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
  let answers = [ ("q2: P(fh)", fhPrior)
                , ("q3: P(fs|fh,fm)", fsGivenHowlMoon)
                , ("q4: P(fs|fh,fm,fb)", fsGivenHowlMoonBowl)
                , ("q5: P(fs|fh,fm,fb,na)", fsGivenHowlMoonBowlAway)
                ]
  -- Print all solutions first.
  forM_ answers $ \(s, (p, _)) -> putStrLn $ s ++ " = " ++ printf "%f" p
  putStr "\n"
  -- Render factor tables.
  mapM_ (uncurry $ flip render) answers


