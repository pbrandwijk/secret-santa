module Lib
    ( assignSanta
    ) where

import Shuffle
import System.IO.Unsafe

-- Preconditions:
--  minimal of three people in the list
--  every name in the list must be unique

-- Program

assignSanta :: [String] -> [(String, String)]
assignSanta s = if noSantaOfSelf assn then assn else assignSanta s
  where
    assn = zip s (unsafePerformIO $ shuffle s)

noSantaOfSelf [] = True
noSantaOfSelf ((s,r):xs) | s == r = False
                         | otherwise = True && noSantaOfSelf xs


-- Postconditions
--  all members occur as santa once
--  all members occur as receiver once
--  there are no santas that are not members
--  there are no receivers that are not members
--  no member is santa to himself
