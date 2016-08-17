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

module Travel where

import DepthFirst
import Problem

data TravelState = TravelState { location :: String
                               , cost     :: Int
                               , moves    :: [String]
                               }
                 deriving (Show)

instance Eq TravelState where
  s1 == s2 = (location s1) == (location s2)

instance Ord TravelState where
  compare s1 s2 = compare (location s1) (location s2)

instance Problem TravelState String where
  actions   = genMoves
  result    = travel
  goal s    = location s == "end"
  stepCost  = \_ _ s -> cost s
  addStates = DepthFirst.addStates

instance DepthFirst TravelState String

mkTravelState :: TravelState
mkTravelState = TravelState "start" 0 ["start"]

genMoves :: TravelState -> [String]
genMoves s = case location s of
                  "start"    -> ["middle-1", "end"]
                  "middle-1" -> ["middle-2"]
                  _          -> ["end"]

travel :: TravelState -> String -> TravelState
travel s m = s { location = m
               , cost = cost s + 1
               , moves = m:(moves s)
               }
