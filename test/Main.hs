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

import Test.Tasty

import qualified Manhattan.Tests as Manhattan
import qualified Swap.Tests as Swap
import qualified Problems.Tiles.Tests as Tiles

main :: IO ()
main = defaultMain $
  testGroup "Unit tests"
  [ Manhattan.tests
  , Swap.tests
  , Tiles.tests
  ]
