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

module Problem where

import qualified Data.Set as Set

class (Ord state, Show state, Show action) => Problem state action | state -> action where
  -- Generate a list of actions that can be performed from a given state
  actions   :: state -> [action]
  -- Apply an action to a state
  result    :: state -> action -> state
  -- Determine whether a state is a goal state
  goal      :: state -> Bool
  -- Calculate the cost of a given step
  stepCost  :: state -> action -> state -> Int
  -- Add states to the frontier
  addStates :: [state] -> [state] -> [state]
  -- Solve the problem from an initial state, returning the goal state
  solve     :: state -> state
  solve s = loop [s] Set.empty

loop :: Problem state action => [state] -> Set.Set state -> state
loop (s:ss) history
  -- Return the steps taken to reach the goal state
  | goal s = s
  | otherwise
    -- Expand the first state on the frontier
    = let expanded = [ result s a | a <- actions s ]
          -- Only keep unique states
          expanded' = filter (\x -> Set.notMember x history) expanded
          -- Add the expanded states to the frontier and loop
          in loop (addStates expanded' ss) (Set.insert s history)
