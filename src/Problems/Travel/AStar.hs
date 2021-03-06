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

{-# OPTIONS_HADDOCK hide #-}

module Problems.Travel.AStar where

import Algorithms.AStar
import Problem
import Problems.Travel.Internal

instance Problem TravelState String where
  actions   = genMoves
  result    = travel
  goal s    = location s == end s
  stepCost  = pathCost
  addStates = Algorithms.AStar.addStates

instance AStar TravelState String where
  heuristic = cost
  scoreCompare s1 s2 = compare (cost s1) (cost s2)
