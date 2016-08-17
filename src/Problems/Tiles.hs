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

module Tiles
  ( mkTilesState
  , moves
  ) where

import AStar
import Data.Array
import Data.List
import Manhattan
import Problem

type Tile = Int
type Move = Tile
type Grid = [Tile]

data TilesState = TilesState { grid  :: Grid
                             , score :: Int
                             , cost  :: Int
                             , moves :: [Move]
                             }
                deriving (Show)

-- Eq and Ord are needed for use with a Set
instance Eq TilesState where
  s1 == s2 = (grid s1) == (grid s2)

instance Ord TilesState where
  compare s1 s2 = compare (grid s1) (grid s2)

-- Implement the Problem and AStar type classes
instance Problem TilesState Move where
  actions   = genMoves
  result    = makeMove
  goal      = complete
  stepCost  = \_ _ s -> cost s
  addStates = AStar.addStates

instance AStar TilesState Move where
  heuristic TilesState { grid = g, cost = c }
    = let distance t = case elemIndex t g of
                            Just index -> manhattan index t (size g)
                            Nothing    -> 0
          in sum (map distance g) + c
  scoreCompare s1 s2 = compare (score s1) (score s2)

mkTilesState :: Grid -> TilesState
mkTilesState g = let zeroState = TilesState g 0 0 []
                     in TilesState g (heuristic zeroState) 0 []

complete :: TilesState -> Bool
complete = sorted . grid

-- Generate a list of possible moves for the given state
genMoves :: TilesState -> [Move]
genMoves TilesState { grid = g }
  = case elemIndex 0 g of
         Just index -> genMoves' g index (size g)
         Nothing    -> []

genMoves' :: Grid -> Int -> Int -> [Move]
genMoves' grid index size
  -- The 0 is on the left side
  | index `mod` size == 0
    = filter inBounds [ north, east, south ]
  -- The 0 is on the right side
  | (index + 1) `mod` size == 0
    = filter inBounds [ north, south, west ]
  -- Otherwise bounds check will remove north and south
  | otherwise
    = filter inBounds [ north, east, south, west ]
  where inBounds direction = direction >= 0 && direction < size ^ 2
        north = index - size
        east  = index + 1
        south = index + size
        west  = index - 1

-- Perform the given move, returning a new fully updated state
makeMove :: TilesState -> Move -> TilesState
makeMove state@TilesState { grid = g } m
  = case elemIndex 0 g of
         Just index -> let newState = state { grid = swap index m g, 
                                              cost = (cost state) + 1,
                                              moves = m : (moves state) }
                           in newState { score = heuristic newState }
         Nothing    -> state

sorted :: Ord a => [a] -> Bool
sorted [ ] = True
sorted [x] = True
sorted (x:y:xs) = x < y && sorted (y:xs)

size :: Grid -> Int
size = floor . sqrt . fromIntegral . length

swap :: Int -> Int -> [a] -> [a]
swap x y as = let asArray = listArray (0, length as - 1) as
                  in elems $ asArray // [(x, asArray!y), (y, asArray!x)]
