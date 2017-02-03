--------------------------------------------------------------------------
--- Library for roman numbers.
--- This library defines a datatype `Roman` to represent roman numbers,
--- conversion operations `fromInt` and `toInt` to convert
--- integers into roman numbers and back, and an operation `showRoman`
--- to show roman numbers as strings.
--- 
--- @author Bernd Brassel, Michael Hanus
--- @version 0.0.1
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module Roman (Roman(..), showRoman, showsRoman, toInt, fromInt, regular)
 where

import ShowS
import Test.Prop

--- The type to represent roman numbers.
--- Examples: IV = Minus I V, III is = Plus I (Plus I)

data Roman = I | V | X | L | C | D | M
           | Minus Roman Roman 
           | Plus  Roman Roman

--- Is a given roman number well formed?
regular :: Roman -> Bool
regular r = fromInt (toInt r) == r

--- Shows roman numbers in the usual way.
showRoman :: Roman -> String
showRoman r = showsRoman r ""

--- Shows roman numbers continuation style.
showsRoman :: Roman -> ShowS
showsRoman r = case r of
  Minus m n       -> showsRoman m . showsRoman n
  Plus  n p       -> showsRoman n . showsRoman p
  _               -> shows r

--- Converts a roman number to integer.
toInt :: Roman -> Int
toInt I   =    1
toInt V   =    5
toInt X   =   10
toInt L   =   50
toInt C   =  100
toInt D   =  500
toInt M   = 1000
toInt (Minus m r)       = toInt r - toInt m
toInt (Plus  r p)       = toInt r + toInt p

--- Converts integers to roman numbers.
--- Since we have no over/underlining, we stop with 3999.
fromInt :: Int -> Roman
fromInt i | 1 <= i && i <= 4000 = foldr1 Plus (base i [M,C,X,I])

base :: Int -> [Roman] -> [Roman]
base _ [] = []
base i (r:rs) 
  | 9 <= dR = Minus (times (dR-8)) (ten r) : rest
  | 6 <= dR = Plus  (five r) (times (dR-5)) : rest
  | 5 == dR = five r : rest
  | 4 == dR = Minus r (five r) : rest
  | 1 <= dR = times dR : rest
  | otherwise = rest
  where
    (dR,mR) = divMod i (toInt r)
    times t = foldr1 Plus $ replicate t r
    rest = base mR rs
    
five :: Roman -> Roman
five C = D
five X = L
five I = V

ten :: Roman -> Roman
ten C = M
ten X = C
ten I = X

------------------------------------------------------------------------------
-- Some tests.

-- Test for correct conversion of roman numbers.
allRegular :: Int -> Prop
allRegular n = n>=1 && n<=4000 ==> toInt (fromInt n) -=- n

-- Some specific roman numbers:
roman42 :: Prop
roman42 = showRoman (fromInt 42) -=- "XLII"

roman1963 :: Prop
roman1963 = showRoman (fromInt 1963) -=- "MCMLXIII"

------------------------------------------------------------------------------
