module Main where

import Prelude hiding ((^))
import Factor
import Bayes

import Control.Monad.Writer

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
  term $ BayesTerm (BayesAssgs $ (var, not val):vs) p

problem :: [BayesTerm]
problem = snd . runWriter $ do
  termWithNeg $ P(fs) .= 0.05
  --termWithNeg $ P(-fs) .= 0.95
  termWithNeg $ P(fb .|. fs) .= 0.6
  termWithNeg $ P(fb .|. -fs) .= 0.1
  termWithNeg $ P(fm) .= 0.0357
  --termWithNeg $ P(-fm) .= 0.9643
  termWithNeg $ P(na) .= 0.3
  --termWithNeg $ P(-na) .= 0.7
  termWithNeg $ P(ndh .|. na ^ fm) .= 0.8
  termWithNeg $ P(ndh .|. na ^ -fm) .= 0.5
  termWithNeg $ P(ndh .|. -na ^ fm) .= 0.4
  termWithNeg $ P(ndh .|. -na ^ -fm) .= 0
  termWithNeg $ P(fh .|. fs ^ ndh ^ fm) .= 0.99
  termWithNeg $ P(fh .|. fs ^ ndh ^ -fm) .= 0.75
  termWithNeg $ P(fh .|. fs ^ -ndh ^ fm) .= 0.9
  termWithNeg $ P(fh .|. fs ^ -ndh ^ -fm) .= 0.5
  termWithNeg $ P(fh .|. -fs ^ ndh ^ fm) .= 0.65
  termWithNeg $ P(fh .|. -fs ^ ndh ^ -fm) .= 0.2
  termWithNeg $ P(fh .|. -fs ^ -ndh ^ fm) .= 0.4
  termWithNeg $ P(fh .|. -fs ^ -ndh ^ -fm) .= 0

q2 :: Prob
q2 = compute problem (P(fh)) [] []

main :: IO ()
main = print q2

