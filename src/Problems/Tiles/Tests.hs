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

import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit

import Algorithms.AStar
import Problems.Tiles.Internal

goalState :: TilesState
goalState = mkTilesState [0..8]

nonGoalState :: TilesState
nonGoalState = mkTilesState (reverse [0..8])

tests :: TestTree
tests = testGroup "Problems.Tiles Tests"
  [ mkTilesStateInitial
  , tilesComplete
  , heuristicTest
  , genMovesTest
  , moveMade
  ]

mkTilesStateInitial :: TestTree
mkTilesStateInitial = testGroup "mkTilesState initial value"
  [ testCase "New state has cost 0" $
    cost nonGoalState @?= 0
  , testCase "New state has no move history" $
    moves nonGoalState @?= []
  ]

tilesComplete :: TestTree
tilesComplete = testGroup "TilesState complete or not"
  [ testCase "Goal state is complete" $
    complete goalState @?= True
  , testCase "Non-goal state is not complete" $
    complete nonGoalState @?= False
  ]

heuristicTest :: TestTree
heuristicTest = testGroup "Heuristic"
  [ testCase "Goal state with cost = 0 has score = 0" $
    heuristic goalState @?= 0
  , testCase "Goal state with cost > 0 has score = cost" $
    heuristic (goalState { cost = 5 }) @?= 5
  , testCase "Score - cost = sum of manhattan distances" $
    heuristic (mkTilesState
                [ 0, 2, 1
                , 3, 8, 4
                , 5, 7, 6 ])
              @?= 0 + 1 + 1
                + 0 + 2 + 1
                + 3 + 0 + 2
  ]

genMovesTest :: TestTree
genMovesTest = testGroup "Generating moves"
  [ testCase "Move in all four directions" $
    sort (genMoves (mkTilesState [ 1, 2, 3
                                 , 4, 0, 5
                                 , 6, 7, 8 ]))
    @?= [ 1, 3, 5, 7 ]
  , testCase "Move in three directions" $
    sort (genMoves (mkTilesState [ 1, 0, 2
                                 , 3, 4, 5
                                 , 6, 7, 8 ]))
    @?= [ 0, 2, 4 ]
  , testCase "Move in two directions" $
    sort (genMoves (mkTilesState [ 0, 1, 2
                                 , 3, 4, 5
                                 , 6, 7, 8 ]))
    @?= [ 1, 3 ]
  ]

moveMade :: TestTree
moveMade = let original = mkTilesState [ 1, 2, 3
                                       , 4, 0, 5
                                       , 6, 7, 8 ]
               next = makeMove original 1
               in testCaseSteps "A move is made" $
                  \step -> do
                    step "The move is applied to the grid"
                    grid next @?= [ 1, 0, 3
                                  , 4, 2, 5
                                  , 6, 7, 8 ]
                    step "The new score is calculated"
                    score next @?= 1 + 1 + 3
                                 + 1 + 2 + 0
                                 + 0 + 0 + 0
                                 + (cost next)
                    step "The cost is incremented by one"
                    cost next @?= cost original + 1
