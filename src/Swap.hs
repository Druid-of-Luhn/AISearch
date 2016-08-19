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

{-|
Module      : Swap
Description : A function to swap two elements in a list.
Copyright   : Copyright © Billy Brown 2016
License     : GPL-3
Maintainer  : druidofluhn@gmail.com
-}
module Swap
  ( swap
  ) where

-- | Swap two elements in a list.
swap :: [a]    -- ^ Input list
        -> Int -- ^ Source index
        -> Int -- ^ Target index
        -> [a] -- ^ Result list
swap [] _ _ = []
swap xs 0 0 = xs
swap (x:xs) 0 t = let (item, rest) = swapHelper x xs (t - 1)
                      in item : rest
swap (x:xs) s t | s > t = swap (x:xs) t s
                | otherwise = x : swap xs (s - 1) (t - 1)

swapHelper :: a -> [a] -> Int -> (a, [a])
swapHelper x [] _ = error "swap index out of bounds"
swapHelper x (y:ys) 0 = (y, x:ys)
swapHelper x (y:ys) t = let (item, rest) = swapHelper x ys (t - 1)
                            in (item, y:rest)
