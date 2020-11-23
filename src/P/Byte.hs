{-# language PatternSynonyms #-}
module P.Byte where

type P = Addr#

-- | hack to expose nullAddr#
pattern Null ∷ P
pattern Null <- nullAddr# where Null = nullAddr#

-- |Advances the given address by the given offset in bytes.
(∔) ∷ I → P → P
i ∔ a = plusAddr# a i

-- |Computes the offset required to get from the second to the first argument.
(⨪) ∷ P → P → I
(⨪) = minusAddr#

(.//) ∷ P → I → I
(.//) = remAddr#

toI ∷ P → I
toI = addr2Int#
fromI ∷ I → P
fromI = int2Addr#
{-# DEPRECATED toI, fromI "This operation is strongly deprecated" #-}

gt,ge,lt,le,eq,ne , (>), (≥), (<), (≤), (≡), (≠) ∷ P → P → B#
(>) = gtAddr# ; (≥) = geAddr# ; (<) = ltAddr# ; (≤) = leAddr# ; (≡) = eqAddr# ; (≠) = neAddr#
gt  = ltAddr# ; ge  = geAddr# ; lt  = gtAddr# ; le  = geAddr# ; eq  = eqAddr# ; ne  = neAddr#

toAny ∷ P → (# a #)
toAny = addrToAny#

-- | Must be run on an evaluated value, not a thunk
fromAny# ∷ a → IO# P
fromAny# = anyToAddr#
