module AStar where

import Data.List
import Problem

class (Problem state action) => AStar state action where
  -- An admissible heuristic scoring function
  heuristic    :: state -> Int
  -- An A* state must be able to have its score compared
  scoreCompare :: state -> state -> Ordering
  -- A* adds to the frontier sorted by score
  addStates    :: [state] -> [state] -> [state]
  addStates  [] ys = ys
  addStates  (x:xs) ys
    = Problem.addStates xs $ insertBy scoreCompare x ys
