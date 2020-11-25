module Char (Char, pattern Char ,module Char) where

-- | 31-bit Unicode code points
type Char = Char#

(>),(≥),(<),(≤),
  gt,ge,lt,le,eq,ne ∷ Char → Char → B#
(>) = gtChar#; (≥) = geChar#; (<) = ltChar#; (≤) = leChar#
instance (≡) Char where (≡) = eqChar#; (≠) = neChar#
gt = ltChar#; ge = leChar#; lt = gtChar#; le = geChar#
eq = eqChar#; ne = neChar#

pattern Char ∷ I → Char
pattern Char{toI} ← (ord# → toI) where Char = chr#
