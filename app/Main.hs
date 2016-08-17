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

module Main where

import System.Environment

import TilesMain
import TravelBFSMain
import TravelDFSMain

main :: IO ()
main = do
  args <- getArgs
  if length args > 0
     then chooseProblem (head args)
     else putStrLn "Please provide the problem name."

chooseProblem :: String -> IO ()
chooseProblem "8tiles" = tiles
chooseProblem "travel-bfs" = travelBFS
chooseProblem "travel-dfs" = travelDFS
chooseProblem _ = putStrLn "Problem not implemented yet."
