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

{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Manhattan
Description : A polymorphic definition and implementation for calculating the Manhattan Block Distance between two points.
Copyright   : Copyright © Billy Brown 2016
License     : GPL-3
Maintainer  : druidofluhn@gmail.com
-}
module Manhattan where

-- | 'ManhattanBlock' defines the function 'manhattan' for calculating the distance between two points.
class ManhattanBlock a where
  -- | Calculate the manhattan block distance between two points.
  manhattan :: a   -- ^ x
            -> a   -- ^ y
            -> Int -- ^ Grid width for 1d array indexing.
            -> Int -- ^ Manhattan block distance.

-- | If the points are integers, they are interpreted as array indices.
instance ManhattanBlock Int where
  manhattan index target width
  -- Calculate the coordinates
    = let x = index `mod` width
          y = index `div` width
          targetX = target `mod` width
          targetY = target `div` width
          -- Find the Manhattan block distance between them
          in manhattan (x, y) (targetX, targetY) width

-- | If the points are tuples of integers, they are interpreted as cartesian coordinates.
instance ManhattanBlock (Int, Int) where
  manhattan (x, y) (tx, ty) _ = abs (tx - x) + abs (ty - y)
