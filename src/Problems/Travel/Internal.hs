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

module Problems.Travel.Internal where

import qualified Data.Map.Strict as Map
import Problem

data TravelState = TravelState { location :: String
                               , end      :: String
                               , graph    :: Map.Map String [(String, Int)]
                               , cost     :: Int
                               , moves    :: [String]
                               }
                 deriving (Show)

instance Eq TravelState where
  s1 == s2 = (location s1) == (location s2)

instance Ord TravelState where
  compare s1 s2 = compare (location s1) (location s2)

mkTravelState :: [String] -> TravelState
mkTravelState (l:ls)
  = let travelGraph = mkTravelGraph ls
        parts = words l
        in if length parts == 2
              then TravelState (parts !! 0) (parts !! 1) travelGraph 0 [parts !! 0]
              else TravelState "end" "end" travelGraph 0 ["end"]

mkTravelGraph :: [String] -> Map.Map String [(String, Int)]
mkTravelGraph ls = mkTravelGraph' ls Map.empty
  where mkTravelGraph' [] m = m
        -- Add the current line to the map, then recurse on the rest
        mkTravelGraph' (l:ls) m = mkTravelGraph' ls (addLine (words l) m)

        -- The key is the first entry, the value is an array of tuples of the rest
        addLine (w:ws) m = Map.insert w (zip (takeOdd ws) (map read $ takeEven ws)) m
        -- Take the odd-indexed values
        takeOdd [] = []
        takeOdd (a:b:xs) = a : (takeOdd xs)
        -- Take the even-indexed values
        takeEven [] = []
        takeEven (a:b:xs) = b : (takeEven xs)

genMoves :: TravelState -> [String]
genMoves s = case Map.lookup (location s) (graph s) of
                  Just next -> map (\(x, _) -> x) next
                  Nothing   -> []

travel :: TravelState -> String -> TravelState
travel s m = s { location = m
               , cost = cost s + pathCost s m s
               , moves = m:(moves s)
               }

pathCost :: TravelState -> String -> TravelState -> Int
pathCost s m _ = case Map.lookup (location s) (graph s) of
                      Just next -> getCost next m
                      Nothing   -> 0
  where getCost [] _ = 0
        getCost ((name, cost):xs) move
          | name == move = cost
          | otherwise = getCost xs move
