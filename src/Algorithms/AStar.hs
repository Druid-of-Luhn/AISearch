-- Copyright © Billy Brown 2016
--
-- This file is part of AISearch.
--
-- AISearch is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- AISearch is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with AISearch.  If not, see <http://www.gnu.org/licenses/>.

{-|
Module      : Algorithms.AStar
Description : Functions for A* search and an implementation of the 'addStates' function.
Copyright   : Copyright © Billy Brown 2016
License     : GPL-3
Maintainer  : druidofluhn@gmail.com

The A* search algorithm uses an admissible heuristic to order states by best first, taking the cost so far into account in order to prevent endless loops. It finds the cheapest path to a solution in an often small amount of steps.
-}
module Algorithms.AStar where

import Data.List
import Problem

-- | Extend the 'Problem' class with an implementation of 'addStates' and specific A* functions.
class (Problem state action) => AStar state action where
  -- | An admissible heuristic scoring function.
  heuristic    :: state -> Int
  -- | An A* state must be able to have its score compared for 'addStates'.
  scoreCompare :: state -> state -> Ordering
  -- | A* adds to the frontier sorted by score.
  addStates    :: [state] -> [state] -> [state]
  addStates [] ys = ys
  addStates (x:xs) ys
    = Problem.addStates xs $ insertBy scoreCompare x ys
