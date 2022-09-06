module P where

-- | hack to expose nullAddr#
pattern Null ∷ P#
pattern Null ← nullAddr# where Null = nullAddr#

-- |Advances the given address by the given offset in bytes.
(∔) ∷ P# -> I -> P#
(∔) = plusAddr#

-- |Computes the offset required to get from the second to the first argument.
(߸) ∷ P# → P# → I
(߸) = minusAddr#

(.//) ∷ P# → I → I
(.//) = remAddr#
