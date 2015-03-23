
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
  putStrLn "./decision-tree [--dot out.dot] file.txt feature1 feature2 feature3..."
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

parseAndLearnTree :: String -> [String] -> IO ([Example], DecisionTree)
parseAndLearnTree filename attrNames = do
  file <- readFile filename
  case parseExamples file of
    Left err -> putStrLn err >> exitFailure
    Right examples -> return (examples, learnDecisionTree examples [0..length attrNames] "<none>")

main :: IO ()
main = do
  args <- getArgs
  let is_dot = head args == "--dot"
      dot_filename = args !! 1
  when (not is_dot && length args < 2) usage
  when (is_dot && length args < 4) usage

  let filename | is_dot = args !! 2
               | otherwise = head args
      attrNames | is_dot = drop 3 args
                | otherwise = tail args
  (examples, tree) <- parseAndLearnTree filename attrNames

  when is_dot $ do
    putStrLn $ "Writing dot file " ++ dot_filename ++ "."
    writeFile dot_filename $ decisionTreeToDot tree attrNames
  let checkExample ex = exClass (classify tree $ exAttrs ex) == exClass ex
      wrongExamples = filter (not . checkExample) examples
  if null wrongExamples
    then putStrLn "Tree worked on all examples!"
    else do
      putStrLn "Incorrectly Classified: "
      print wrongExamples


