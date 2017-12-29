module Lib
    ( assignSanta
    ) where

import Shuffle (shuffle)
import System.IO.Unsafe (unsafePerformIO)
import Data.List.Unique (allUnique)

-- PRECONDITIONS

-- Minimal of three people in the list
minimalThree :: [String] -> Bool
minimalThree xs = length xs >= 3

-- Every name in the list must be unique
everyNameUnique :: [String] -> Bool
everyNameUnique = allUnique



-- PROGRAM

assignSanta :: [String] -> [(String, String)]
assignSanta s = if nonReflexive assn then assn else assignSanta s
  where
    assn = zip s (unsafePerformIO $ shuffle s)

-- Relation is non-reflexive; no one is santa of himself.
nonReflexive :: (Eq a) => [(a,a)] -> Bool
nonReflexive [] = True
nonReflexive ((s,r):xs) | s /= r = True && nonReflexive xs
                        | otherwise = False


-- POSTCONDITIONS

-- All members occur as santa once (if (x,y), then no (x,z))
-- All members occur as receiver once (if (x,y), then no (z,y))
-- There are no santas that are not members (if (x,y), then also (z,x))
-- There are no receivers that are not members (if (x,y), then also (y,z))
-- No member is santa to himself (no (x,x))
-- ?No two members are santa to each other (if (x,y), then no (y,x))
