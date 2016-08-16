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
