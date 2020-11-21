{-# language PatternSynonyms #-}
module Ref.Byte where

type Ref = Addr#

-- | hack to expose nullAddr#
pattern Null ∷ Ref
pattern Null <- nullAddr# where Null = nullAddr#

-- |Advances the given address by the given offset in bytes.
(∔) ∷ I → Ref → Ref
i ∔ a = plusAddr# a i

-- |Computes the offset required to get from the second to the first argument.
(⨪) ∷ Ref → Ref → I
(⨪) = minusAddr#

(.//) ∷ Ref → I → I
(.//) = remAddr#

toI ∷ Ref → I
toI = addr2Int#
fromI ∷ I → Ref
fromI = int2Addr#
{-# DEPRECATED toI, fromI "This operation is strongly deprecated" #-}

gt,ge,lt,le,eq,ne , (>), (≥), (<), (≤), (≡), (≠) ∷ Ref → Ref → B#
(>) = gtAddr# ; (≥) = geAddr# ; (<) = ltAddr# ; (≤) = leAddr# ; (≡) = eqAddr# ; (≠) = neAddr#
gt  = ltAddr# ; ge  = geAddr# ; lt  = gtAddr# ; le  = geAddr# ; eq  = eqAddr# ; ne  = neAddr#

toAny ∷ Ref → (# a #)
toAny = addrToAny#

-- | Must be run on an evaluated value, not a thunk
fromAny# ∷ a → IO# Ref
fromAny# = anyToAddr#
