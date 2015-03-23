
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when, forM)

import Data.List.Split (endByOneOf)
import Safe (readMay)
import qualified Data.Vector.Unboxed as V

import DecisionTree

usage :: IO ()
usage = do
  putStrLn "./decision-tree file.txt feature1 feature2 feature3..."
  exitFailure

parseExamples :: String -> Either String [Example]
parseExamples file =
  forM (lines file) $ \line -> do
    let pieces = endByOneOf ",." $ init line
        attrs = init pieces
        outcome = last pieces
    parsedAttrs <- forM attrs $ \f_string ->
      case readMay f_string :: Maybe Float of
        Nothing -> Left $ "Failed to parse " ++ f_string ++ " as a float."
        Just f -> Right f
    return $ Example (V.fromList parsedAttrs) outcome

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) usage
  let filename = head args
      attrNames = tail args
  file <- readFile filename
  case parseExamples file of
    Left err -> putStrLn err >> exitFailure
    Right examples ->
      let tree = learnDecisionTree examples [0..length attrNames] "<none>"
      in print "yay"


