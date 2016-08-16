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

module Manhattan where

class ManhattanBlock a where
  manhattan :: a -> a -> Int -> Int

instance ManhattanBlock Int where
  manhattan index target width
  -- Calculate the coordinates
    = let x = index `mod` width
          y = index `div` width
          targetX = target `mod` width
          targetY = target `div` width
          -- Find the Manhattan block distance between them
          in manhattan (x, y) (targetX, targetY) width

instance ManhattanBlock (Int, Int) where
  manhattan (x, y) (tx, ty) _ = abs (tx - x) + abs (ty - y)
