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
assignSanta s = if nonReflexive assn then assn else assignSanta s
  where
    assn = zip s (unsafePerformIO $ shuffle s)

-- Relation is non-reflexive; no one is santa of himself.
nonReflexive :: (Eq a) => [(a,a)] -> Bool
nonReflexive [] = True
nonReflexive ((s,r):xs) | s /= r = True && nonReflexive xs
                        | otherwise = False


-- Postconditions
--  all members occur as santa once (if (x,y), then no (x,z))
--  all members occur as receiver once (if (x,y), then no (z,y))
--  there are no santas that are not members (if (x,y), then also (z,x))
--  there are no receivers that are not members (if (x,y), then also (y,z))
--  no member is santa to himself (no (x,x))
-- ?no two members are santa to each other (if (x,y), then no (y,x))
