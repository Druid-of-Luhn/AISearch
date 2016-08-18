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
  case args of
       [arg]           -> chooseProblem arg
       [arg, "--help"] -> help arg
       _               -> putStrLn "Usage: stack exec AISearch-exe -- <problem> [--help]"

chooseProblem :: String -> IO ()
chooseProblem "8tiles" = tiles
chooseProblem "travel-bfs" = travelBFS
chooseProblem "travel-dfs" = travelDFS
chooseProblem p = putStrLn $ "Problem '" ++ p ++ "' not implemented."

help :: String -> IO ()
help _ = putStrLn "Help not implemented yet."
