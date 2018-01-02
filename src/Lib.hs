module Lib where

import Shuffle (shuffle)
import System.IO.Unsafe (unsafePerformIO)
import Data.List.Unique (allUnique)
import Data.List (nub)

-- PRECONDITIONS

-- Minimal of three people in the list
minimalThree :: [String] -> Bool
minimalThree xs = length xs >= 3

-- Every name in the list must be unique
everyNameUnique :: [String] -> Bool
everyNameUnique = allUnique



-- PROGRAM

checkPreconditions :: [String] -> Bool
checkPreconditions xs =
  if minimalThree xs == False then error "Group must have at least three members!"
  else if everyNameUnique xs == False then error "All member names must be unique!"
  else True

assignSanta :: [String] -> [(String, String)]
assignSanta s = if nonReflexive assn then assn else assignSanta s
  where
    assn = zip s (unsafePerformIO $ shuffle s)

checkPostconditions :: [(String,String)] -> Bool
checkPostconditions xs =
  if nonReflexive xs == False then error "No member can be santa to himself!"
  else True


-- POSTCONDITIONS

-- All members occur as santa once (if (x,y), then no (x,z))
domainMembersUnique :: (Eq a) => [(a,a)] -> Bool
domainMembersUnique xs = (nub domain) == domain
  where
    domain = map fst xs

-- All members occur as receiver once (if (x,y), then no (z,y))

-- There are no santas that are not members (if (x,y), then also (z,x))

-- There are no receivers that are not members (if (x,y), then also (y,z))

-- Relation is non-reflexive; no one is santa of himself (no (x,x))
nonReflexive :: (Eq a) => [(a,a)] -> Bool
nonReflexive [] = True
nonReflexive ((s,r):xs) | s /= r = True && nonReflexive xs
                        | otherwise = False

-- ?No two members are santa to each other (if (x,y), then no (y,x))
