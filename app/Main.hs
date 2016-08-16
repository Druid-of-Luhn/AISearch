module Main where

import System.Environment

import AStar
import Problem
import Tiles

main :: IO ()
main = do
  args <- getArgs
  if length args > 0
     then chooseProblem (head args)
     else putStrLn "Please provide the problem name."

chooseProblem :: String -> IO ()
chooseProblem "8tiles"
  = do
      -- Read the problem from stdin
      input <- getContents
      -- Convert the input to Ints, solve and print the moves taken
      print $ reverse $ moves $ solve (mkTilesState (map read (words input)))
chooseProblem _ = putStrLn "Problem not implemented yet."
