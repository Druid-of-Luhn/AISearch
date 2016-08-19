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

module ManhattanTest where

import Manhattan
import TestRunner

test :: IO ()
test = do putStrLn ""
          singleDimensionTestZero
          singleDimensionTestMax
          doubleDimensionTestZero
          doubleDimensionTestMax

singleDimensionTestZero
  = runTest "Manhattan.singleDimensionTestZero"
            (manhattan (2 :: Int)
                       (2 :: Int)
                       3)
            0

singleDimensionTestMax
  = runTest "Manhattan.singleDimensionTestMax"
            (manhattan (0 :: Int)
                       (8 :: Int)
                       3)
            4

doubleDimensionTestZero
  = runTest "Manhattan.doubleDimensionTestZero"
            (manhattan ((1, 1) :: (Int, Int))
                       ((1, 1) :: (Int, Int))
                       3)
            0

doubleDimensionTestMax
  = runTest "Manhattan.doubleDimensionTestMax"
            (manhattan ((0, 0) :: (Int, Int))
                       ((2, 2) :: (Int, Int))
                       3)
            4
