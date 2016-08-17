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
  addStates [] ys = ys
  addStates (x:xs) ys
    = Problem.addStates xs $ insertBy scoreCompare x ys
