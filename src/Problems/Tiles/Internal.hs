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

module Problems.Tiles.Internal where

import qualified Data.Vector as V

import Algorithms.AStar
import Manhattan
import Problem
import Swap

type Tile = Int
type Move = Tile
type Grid = V.Vector Tile

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
  addStates = Algorithms.AStar.addStates

instance AStar TilesState Move where
  heuristic TilesState { grid = g, cost = c }
    = let distance t = case V.elemIndex t g of
                            Just index -> manhattan index t (size g)
                            Nothing    -> 0
          in V.sum (V.map distance g) + c
  scoreCompare s1 s2 = compare (score s1) (score s2)

mkTilesState :: Grid -> TilesState
mkTilesState g = let zeroState = TilesState g 0 0 []
                     in TilesState g (heuristic zeroState) 0 []

complete :: TilesState -> Bool
complete = sorted . grid

-- Generate a list of possible moves for the given state
genMoves :: TilesState -> [Move]
genMoves TilesState { grid = g }
  = case V.elemIndex 0 g of
         Just index -> genMoves' index (size g)
         Nothing    -> []

genMoves' :: Int -> Int -> [Move]
genMoves' index size
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
  = case V.elemIndex 0 g of
         Just index -> let newState = state { grid = swapV g index m, 
                                              cost = (cost state) + 1,
                                              moves = m : (moves state) }
                           in newState { score = heuristic newState }
         Nothing    -> state

sorted :: Ord a => V.Vector a -> Bool
sorted v | V.null v = True
         | V.null $ V.tail v = True
         | otherwise
           = and $ V.map (\(x, y) -> x <= y) $ V.zip v (V.tail v)

size :: Grid -> Int
size = floor . sqrt . fromIntegral . V.length

swapV :: V.Vector a -> Int -> Int -> V.Vector a
swapV v x y = v V.// [(x, v V.! y), (y, v V.! x)]
