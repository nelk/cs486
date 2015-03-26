module DecisionTree where

import Data.Vector.Unboxed (Vector, (!))
import Data.List (partition, maximumBy, sort)
import Data.Ord (comparing)
import Control.Monad.State
import Control.Monad.Writer

-- |Types for this domain.
type Attribute = Int
type FeatureName = String
type Clss = String
type Threshold = Float
data Example = Example
  { exAttrs :: Vector Threshold
  , exClass :: Clss
  }
  deriving (Show, Eq)

-- |Decision Tree data structure.
-- Either a leaf, or a branch that divides on an attribute and threshold.
-- The left child represents values less than or equal to the threshold.
data DecisionTree = Leaf Clss
                  | Branch Attribute Threshold DecisionTree DecisionTree

-- |Are all elements in this list equal?
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (a:b:rest) | a /= b = False
                    | otherwise = allEqual (b:rest)

log2 :: Float -> Float
log2 = logBase 2

lengthF :: [a] -> Float
lengthF = fromIntegral . length

-- |Partition examples by their classification.
partitionByClass :: [Example] -> ([Example], [Example])
partitionByClass examples =
  let a_class = exClass $ head examples
  in partition ((== a_class) . exClass) examples

-- |Partition examples by an attribute and threshold.
partitionByAttr :: Attribute -> Threshold -> [Example] -> ([Example], [Example])
partitionByAttr attr thresh =
  partition (\e -> exAttrs e ! attr <= thresh)

-- |Boolean entropy formula for a probability q.
boolEntropy :: Float -> Float
boolEntropy q = -(q * log2 q + (1 - q) * log2 (1 - q))

-- |Entropy of a group of examples.
entropy :: [Example] -> Float
entropy examples =
  -- Partition them by their classification.
  let (positives, negatives) = partitionByClass examples
      -- Special case - if all are positive or all are negative, entropy is 0.
      entr | null positives || null negatives = 0.0
      -- Otherwise it's the boolean entropy of the fraction with are positive or negative.
           | otherwise = boolEntropy $ lengthF positives / lengthF examples
  in entr

-- |Remaining entropy after splitting given examples by given attribute and threshold.
remainder :: [Example] -> Attribute -> Threshold -> Float
remainder examples attr thresh =
  -- Partition by the attribute and threshold.
  let (lefts, rights) = partitionByAttr attr thresh examples
  -- Sum of weighted entropies by their fraction of the original.
  in (lengthF lefts / lengthF examples) * entropy lefts
   + (lengthF rights / lengthF examples) * entropy rights

-- |Information Gain of a group of examples if divided by given attribute and threshold.
informationGain :: Attribute -> Threshold -> [Example] -> Float
informationGain attr thresh examples = entropy examples - remainder examples attr thresh

-- |Most common classification in a group of examples.
pluralityValue :: [Example] -> Clss
pluralityValue examples = let a_class = exClass $ head examples
                              b_class = head $ filter (/= a_class) $ map exClass examples
                              num_a_class = lengthF $ filter ((== a_class) . exClass) examples
                              answer | num_a_class >= lengthF examples / 2 = a_class
                                     | otherwise = b_class
                          in answer

-- |Main Decision Tree learning algorithm.
-- Given training data, possible attributes to split on, and the most common
-- class from the parent, it constructs a decision tree that would classify the
-- given examples correctly.
learnDecisionTree :: [Example] -> [Attribute] -> Clss -> DecisionTree
learnDecisionTree examples attrs parent_class
  -- If no examples remaining, this node becomes a leaf that holds the
  -- classification passed from the parent.
  | null examples = Leaf parent_class
  -- If all have the same classification then this is a leaf of that classification.
  | allEqual (map exClass examples) = Leaf (exClass $ head examples)
  -- Note: Attributes never empty - can be reused.
  | otherwise =
    let -- Calculates information gains for all mid-point thresholds for the given attribute.
        calculateGains :: Attribute -> [(Int, Threshold, Float)]
        calculateGains attr =
          let -- First get the values for this attribute for each example.
              vals = sort $ map ((! attr) . exAttrs) examples
              -- Then sort the values and get the average of each consecutive pair.
              thresholds = map ((1.0/2.0 *) . uncurry (+)) $ zip vals (tail vals)
          -- For each threshold create a tuple of the attribute, the threshold,
          -- and the information gain that would result by choosing them.
          in map (\t -> (attr, t, informationGain attr t examples)) thresholds

        -- Find the best attribute and threshold to split on by calculating information gain for each
        -- threshold for each attribute.
        -- (Reverse attrs in order to prefer earlier attrs for ties for IG  since maximumBy takes last).
        (best_attr, best_thresh, _) =
            maximumBy (comparing (\(_, _, g) -> g)) $ join $ map calculateGains $ reverse attrs
        -- Partition the examples by the best attribute and threshold.
        (lefts, rights) = partitionByAttr best_attr best_thresh examples
        -- Calculate plurality value to pass to children tree nodes.
        this_class = pluralityValue examples
        -- Helper to recurse on given examples.
        recurse_branch exs = learnDecisionTree exs attrs this_class
    -- Construct branch splitting on the attribute and threshold after recursively
    -- creating left and right subtrees with partitioned examples.
    in Branch best_attr best_thresh (recurse_branch lefts) (recurse_branch rights)

-- |Use a decision tree to classify an instance.
classify :: DecisionTree -> Vector Threshold -> Example
classify (Leaf clss) attrs = Example attrs clss
classify (Branch attr thresh left right) attrs
  | attrs ! attr <= thresh = classify left attrs
  | otherwise = classify right attrs


-- |The rest of the functions are used to generate a graphviz .dot file that can be
-- used to render the decision tree.
nodeName :: Int -> String
nodeName id' = "n" ++ show id'

genNode :: Int -> String -> String
genNode id' label = nodeName id' ++ "[label=\"" ++ label ++ "\"];"

decisionTreeToDot :: DecisionTree -> [String] -> String
decisionTreeToDot tree attr_names =
  let state_comp = decisionTreeToDot_ tree attr_names
      writer_comp = runStateT state_comp 1
      (_, dot_lines) = runWriter writer_comp
      header = [ "digraph G {"
               , "rankdir=TB;"
               ]
      footer = ["}"]
  in unlines $ header ++ dot_lines ++ footer

decisionTreeToDot_ :: DecisionTree -> [String] -> StateT Int (Writer [String]) ()
decisionTreeToDot_ (Leaf clss) _ = do
  cur_id <- get
  put $ cur_id + 1
  tell . return $ genNode cur_id clss
decisionTreeToDot_ (Branch attr thresh left right) attr_names = do
  cur_id <- get
  put $ cur_id + 1
  tell . return $ genNode cur_id $ (attr_names !! attr) ++ " <= " ++ show thresh

  let left_child_id = cur_id + 1
  decisionTreeToDot_ left attr_names
  tell . return $ nodeName cur_id ++ "->" ++ nodeName left_child_id ++ "[label=\"T\"];"

  right_child_id <- get
  decisionTreeToDot_ right attr_names
  tell . return $ nodeName cur_id ++ "->" ++ nodeName right_child_id ++ "[label=\"F\"];"


