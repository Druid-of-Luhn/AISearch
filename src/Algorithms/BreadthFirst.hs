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
Module      : Algorithms.BreadthFirst
Description : An implementation of the 'addStates' function for breadth-first search.
Copyright   : Copyright © Billy Brown 2016
License     : GPL-3
Maintainer  : druidofluhn@gmail.com

Breadth-first search adds newly expanded states to the back of the frontier, making it visit a whole layer of the search space before moving on to the next. This type class extends the 'Problem' class with an implementation of 'addStates'.
-}
module Algorithms.BreadthFirst where

import Problem

-- | Extend the 'Problem' class with an implementation of 'addStates'.
class (Problem state action) => BreadthFirst state action where
  -- Breadth First Search always adds states to the back
  addStates :: [state] -> [state] -> [state]
  addStates xs ys = ys ++ xs

