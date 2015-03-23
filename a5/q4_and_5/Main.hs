
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when, forM)
import Data.Char (isSpace)

import Data.List.Split (endByOneOf)
import Safe (readMay)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import DecisionTree

parseExamples :: String -> IO [Example]
parseExamples file =
  forM (lines file) $ \line -> do
    let pieces = endByOneOf ",." $ if isSpace (last line) then init line else line
        attrs = init pieces
        outcome = last pieces
    parsedAttrs <- forM attrs $ \f_string ->
      case readMay f_string :: Maybe Float of
        Nothing -> do
          putStrLn $ "Failed to parse " ++ f_string ++ " as a float."
          exitFailure
        Just f -> return f
    return $ Example (V.fromList parsedAttrs) outcome

parseAndLearnTree :: String -> [String] -> IO ([Example], DecisionTree)
parseAndLearnTree filename attrNames = do
  file <- readFile filename
  examples <- parseExamples file
  putStrLn $ "Learning on " ++ show (length examples) ++ " training examples..."
  return (examples, learnDecisionTree examples [0..length attrNames - 1] "<none>")

testClassifier :: (Vector Float -> Example) -> [Example] -> Int
testClassifier classifier examples =
  let checkExample ex = exClass (classifier $ exAttrs ex) == exClass ex
      wrong_examples = filter (not . checkExample) examples
   in length wrong_examples

usage :: IO ()
usage = do
  putStrLn "./decision-tree [--dot out.dot] trainData.txt testData.txt feature1 feature2 feature3..."
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) usage
  let is_dot = head args == "--dot"
      dot_filename = args !! 1
  when (is_dot && length args < 5) usage

  let args' | is_dot = drop 2 args
            | otherwise = args
      train_filename = head args'
      test_filename = args' !! 1
      attrNames = drop 2 args'

  putStrLn $ "Problem has " ++ show (length attrNames) ++ " attributes."

  putStrLn $ "Training on data from " ++ train_filename ++ "."
  (training_data, tree) <- parseAndLearnTree train_filename attrNames
  putStrLn $ "Testing on data from " ++ test_filename ++ "."
  test_file <- readFile test_filename
  test_data <- parseExamples test_file

  when is_dot $ do
    putStrLn $ "Writing dot file " ++ dot_filename ++ "."
    writeFile dot_filename $ decisionTreeToDot tree attrNames

  let classifier = classify tree
      num_train_wrong = testClassifier classifier training_data
  putStrLn $ "Correctly classified "
          ++ show (length training_data - num_train_wrong) ++ "/" ++ show (length training_data)
          ++ " training examples!"

  let num_test_wrong = testClassifier classifier test_data
  putStrLn $ "Correctly classified "
            ++ show (length test_data - num_test_wrong) ++ "/" ++ show (length test_data)
            ++ " test examples."


