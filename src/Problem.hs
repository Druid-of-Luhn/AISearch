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
