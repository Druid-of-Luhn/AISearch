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

module Problems.Tiles.Tests
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Problems.Tiles.Internal

tests :: TestTree
tests = testGroup "Problems.Tiles Tests"
  [ mkTilesStateInitial
  , tilesComplete
  , swapTest
  ]

mkTilesStateInitial :: TestTree
mkTilesStateInitial = testGroup "mkTilesState initial value"
  [ testCase "New state has cost 0" $
    cost (mkTilesState $ reverse [0..8]) @?= 0
  , testCase "New state has no move history" $
    moves (mkTilesState [0..8]) @?= []
  ]

tilesComplete :: TestTree
tilesComplete = testGroup "TilesState complete or not"
  [ testCase "Goal state is complete" $
    complete (mkTilesState [0..8]) @?= True
  , testCase "Non-goal state is not complete" $
    complete (mkTilesState $ reverse [0..8]) @?= False
  ]

swapTest :: TestTree
swapTest = let lst = [ 1, 2, 3, 4 ]
               in testGroup "Swap elements in list"
                  [ testCase "Swapping two values" $
                    swap 1 2 lst @?= [ 1, 3, 2, 4 ]
                  , testCase "Swapping edge values" $
                    swap 0 3 lst @?= [ 4, 2, 3, 1 ]
                  , testCase "Swapping value with itself" $
                    swap 1 1 lst @?= lst
                  , testCase "Swapping out of bounds element" $
                    swap 0 4 lst @?= lst
                  ]
