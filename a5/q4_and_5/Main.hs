
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Applicative
import Control.Monad
import Data.Char (isSpace)

import Data.List.Split (splitOn)
import Safe (readMay)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Function (fix)

import DecisionTree

-- |Removes trailing whitespace and periods.
trim :: String -> String
trim = fix (\f s -> if not (null s) && ((||) <$> isSpace <*> (=='.') $ last s) then f (init s) else s)

-- |Parse input string as examples, quitting the program with an error if it fails.
parseExamples :: String -> IO [Example]
parseExamples file =
  forM (lines file) $ \line -> do
    let pieces = splitOn "," $ trim line
        attrs = init pieces
        -- Last number is the label.
        outcome = last pieces
    parsedAttrs <- forM attrs $ \f_string ->
      -- Parse as float.
      case readMay f_string :: Maybe Float of
        Nothing -> do
          putStrLn $ "Failed to parse " ++ f_string ++ " as a float."
          exitFailure
        Just f -> return f
    return $ Example (V.fromList parsedAttrs) outcome

-- |Given a filename and names of attributes, learn a decision tree.
parseAndLearnTree :: String -> [String] -> IO ([Example], DecisionTree)
parseAndLearnTree filename attrNames = do
  file <- readFile filename
  examples <- parseExamples file
  putStrLn $ "Learning on " ++ show (length examples) ++ " training examples..."
  return (examples, learnDecisionTree examples [0..length attrNames - 1] "<none>")

-- |Use a given classifier function to classify a bunch of labeled test data
-- and return the number of incorrect classifications.
testClassifier :: (Vector Float -> Example) -> [Example] -> Int
testClassifier classifier examples =
  let checkExample ex = exClass (classifier $ exAttrs ex) == exClass ex
      wrong_examples = filter (not . checkExample) examples
   in length wrong_examples

-- |Print program usage message and exit.
usage :: IO ()
usage = do
  putStrLn "./decision-tree [--dot out.dot] trainData.txt testData.txt feature1 feature2 feature3..."
  exitFailure

-- |Main program.
main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) usage
  -- Get optional name of file to write graphviz output to from argument list.
  let is_dot = head args == "--dot"
      dot_filename = args !! 1
  when (is_dot && length args < 5) usage

  -- Get data filenames and attributes from argument list.
  let args' | is_dot = drop 2 args
            | otherwise = args
      train_filename = head args'
      test_filename = args' !! 1
      attrNames = drop 2 args'

  putStrLn $ "Problem has " ++ show (length attrNames) ++ " attributes."
  putStrLn $ "Training on data from " ++ train_filename ++ "."

  -- Learn on training data.
  (training_data, tree) <- parseAndLearnTree train_filename attrNames

  -- Output graph.
  when is_dot $ do
    putStrLn $ "Writing dot file " ++ dot_filename ++ "."
    writeFile dot_filename $ decisionTreeToDot tree attrNames

  -- Read training data.
  putStrLn $ "Testing on data from " ++ test_filename ++ "."
  test_file <- readFile test_filename
  test_data <- parseExamples test_file

  -- Use decision tree to classify training data.
  let classifier = classify tree
      num_train_wrong = testClassifier classifier training_data
  putStrLn $ "Correctly classified "
          ++ show (length training_data - num_train_wrong) ++ "/" ++ show (length training_data)
          ++ " training examples!"

  -- Use decision tree to classify test data.
  let num_test_wrong = testClassifier classifier test_data
  putStrLn $ "Correctly classified "
            ++ show (length test_data - num_test_wrong) ++ "/" ++ show (length test_data)
            ++ " test examples."

