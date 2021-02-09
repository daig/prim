--------------------------------------------------------------------
-- | Description : 31-bit Unicode code points
--------------------------------------------------------------------
module Prim.Char (Char, pattern Char ,module Prim.Char) where

-- | 31-bit Unicode code points

instance (≤) Char where
  (>) = coerce gtChar#
  (≥) = coerce geChar#
  (<) = coerce ltChar#
  (≤) = coerce leChar#
instance (≡) Char where
  (≡) = coerce eqChar#
  (≠) = coerce neChar#

pattern Char ∷ I → Char
pattern Char{toI} ← (ord# → toI) where Char = chr#
