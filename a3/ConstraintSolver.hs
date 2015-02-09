{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module ConstraintSolver where

import qualified Data.Set as Set

data (Eq id, Ord dom) =>
  Var id dom = AssignedVar id dom
             | UnassignedVar id (Set.Set dom)

varDomainSize :: (Eq id, Ord dom) => Var id dom -> Int
varDomainSize (AssignedVar _ _) = 1
varDomainSize (UnassignedVar _ s) = Set.size s

varId :: (Eq id, Ord dom) => Var id dom -> id
varId (AssignedVar ident _) = ident
varId (UnassignedVar ident _) = ident

class Constraint c id dom where
  included :: c -> Var id dom -> Bool
  restrict :: c -> Var id dom -> [Var id dom] -> Maybe [Var id dom]

data Constraint c id dom => ConstraintProblem c id dom = ConstraintProblem
  { vars :: [Var id dom]
  , constraints :: [c]
  }

