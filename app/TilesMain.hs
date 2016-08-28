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

module TilesMain where

import qualified Data.Vector as V

import Algorithms.AStar
import Problem
import Problems.Tiles

tiles :: IO ()
tiles
  = do
      -- Read the problem from stdin
      input <- getContents
      -- Convert the input to Ints, solve and print the moves taken
      let solution = solve (mkTilesState $ V.fromList (map read (words input)))
      print $ reverse $ moves $ solution
