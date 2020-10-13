module Char (Char,Char8#, module Char) where

(>),(≥),(<),(≤),(≡),(≠), 
  gt,ge,lt,le,eq,ne ∷ Char → Char → B#
(>) = gtChar#; (≥) = geChar#; (<) = ltChar#; (≤) = leChar#
(≡) = eqChar#; (≠) = neChar#
gt = ltChar#; ge = leChar#; lt = gtChar#; le = geChar#
eq = eqChar#; ne = neChar#

fromI ∷ I → Char
fromI = chr#
toI ∷ Char → I
toI = ord#
