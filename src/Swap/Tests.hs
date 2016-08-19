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

module Swap.Tests
  ( tests
  ) where

import Control.Exception (catch, SomeException)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Swap

tests :: TestTree
tests = let lst = [ 1, 2, 3, 4 ]
            in testGroup "Swap elements in a list"
               [ testCase "Swapping two values" $
                 swap lst 1 2 @?= [ 1, 3, 2, 4 ]
               , testCase "Swapping edge values" $
                 swap lst 0 3 @?= [ 4, 2, 3, 1 ]
               , testCase "Swapping value with itself" $
                 swap lst 1 1 @?= lst
               , testCase "Swapping out of bounds element" $
                 catch (do let _ = swap lst 0 4
                           assertFailure "Expected out of bounds error")
                       handler
               ]
  where handler :: SomeException -> Assertion
        handler e = assertString ""
