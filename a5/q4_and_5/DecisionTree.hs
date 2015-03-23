module DecisionTree where

import Data.Vector.Unboxed (Vector, (!))
import Data.List (partition, maximumBy, sort)
import Data.Ord (comparing)
import Control.Monad.State
import Control.Monad.Writer

type Attribute = Int
type FeatureName = String
type Clss = String
data Example = Example
  { exAttrs :: Vector Float
  , exClass :: Clss
  }
  deriving (Show, Eq)

data DecisionTree = Leaf Clss
                  | Branch Attribute Float DecisionTree DecisionTree

allEqual :: Eq a => [a] -> Bool
allEqual as | length as < 2 = True
allEqual (a:b:rest) | a /= b = False
                    | otherwise = allEqual (b:rest)

log2 :: Float -> Float
log2 = logBase 2

lengthF :: [a] -> Float
lengthF = fromIntegral . length

partitionByClass :: [Example] -> ([Example], [Example])
partitionByClass examples =
  let a_class = exClass $ head examples
  in partition ((== a_class) . exClass) examples

partitionByAttr :: Attribute -> Float -> [Example] -> ([Example], [Example])
partitionByAttr attr thresh examples =
  partition (\e -> exAttrs e ! attr <= thresh) examples

boolEntropy :: Float -> Float
boolEntropy q = -(q * log2 q + (1 - q) * log2 (1 - q))

entropy :: [Example] -> Float
entropy examples =
  let (positives, negatives) = partitionByClass examples
      entr | null positives || null negatives = 0.0
              | otherwise = boolEntropy $ lengthF positives / lengthF examples
  in entr

remainder :: [Example] -> Attribute -> Float -> Float
remainder examples attr thresh =
  let (lefts, rights) = partitionByAttr attr thresh examples
  in (lengthF lefts / lengthF examples) * entropy lefts
   + (lengthF rights / lengthF examples) * entropy rights

informationGain :: Attribute -> Float -> [Example] -> Float
informationGain attr thresh examples = entropy examples - remainder examples attr thresh

pluralityValue :: [Example] -> Clss
pluralityValue examples = let a_class = exClass $ head examples
                              b_class = head $ filter (/= a_class) $ map exClass examples
                              num_a_class = lengthF $ filter ((== a_class) . exClass) examples
                              answer | num_a_class >= lengthF examples / 2 = a_class
                                     | otherwise = b_class
                          in answer

learnDecisionTree :: [Example] -> [Attribute] -> Clss -> DecisionTree
learnDecisionTree examples attrs parent_class
  | null examples = Leaf parent_class
  | allEqual (map exClass examples) = Leaf (exClass $ head examples)
  -- Note: Attributes never empty - can be reused.
  | otherwise =
    let calculateGains :: Attribute -> [(Int, Float, Float)]
        calculateGains attr =
          let vals = sort $ map ((! attr) . exAttrs) examples
              thresholds = map ((1.0/2.0 *) . uncurry (+)) $ zip vals (tail vals)
          in map (\t -> (attr, t, informationGain attr t examples)) thresholds

        (best_attr, best_thresh, _) =
            maximumBy (comparing (\(_, _, g) -> g)) $ join $ map calculateGains attrs
        (lefts, rights) = partitionByAttr best_attr best_thresh examples
        this_class = pluralityValue examples
        recurse_branch exs = learnDecisionTree exs attrs this_class
    in Branch best_attr best_thresh (recurse_branch lefts) (recurse_branch rights)


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
  tell . return $ (nodeName cur_id) ++ "->" ++ (nodeName left_child_id) ++ "[label=\"T\"];"

  right_child_id <- get
  decisionTreeToDot_ right attr_names
  tell . return $ (nodeName cur_id) ++ " -> " ++ (nodeName right_child_id) ++ "[label=\"F\"];"


