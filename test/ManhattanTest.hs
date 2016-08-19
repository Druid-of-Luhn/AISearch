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

module ManhattanTest
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Manhattan

tests :: TestTree
tests = testGroup "Manhattan Tests"
  [ testGroup "Single dimension array"
    [ singleDimensionZero
    , singleDimensionMax ]
  , testGroup "Double dimension array"
    [ doubleDimensionZero
    , doubleDimensionMax ] ]

singleDimensionZero
  = let index = 2 :: Int
        target = index
        width = 3
        in testCase "Distance is zero" $
           manhattan index target width @?= 0

singleDimensionMax
  = let index = 8 :: Int
        target = 0 :: Int
        width = 3
        in testCase "Distance is maximal" $
           manhattan index target width @?= 4

doubleDimensionZero
  = let index = (1, 1) :: (Int, Int)
        target = index
        width = 3
        in testCase "Distance is zero" $
           manhattan index target width @?= 0

doubleDimensionMax
  = let index = (2, 2) :: (Int, Int)
        target = (0, 0) :: (Int, Int)
        width = 3
        in testCase "Distance is maximal" $
           manhattan index target width @?= 4
