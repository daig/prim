--------------------------------------------------------------------
-- | Description : Raw unmanaged pointers
--------------------------------------------------------------------
{-# language TypeSynonymInstances,UnliftedNewtypes, GADTs, TypeOperators #-}
module P (P,module P) where
import Char
import Char8
import I32 (I32(..))
import I16 (I16(..))
import I64 (I64(..))
import I8 (I8(..))
import qualified P.Stable as Stable


-- | hack to expose nullAddr#
pattern Null ∷ P
pattern Null ← nullAddr# where Null = nullAddr#

-- |Advances the given address by the given offset in bytes.
(∔) ∷ I → P → P
i ∔ a = plusAddr# a i

-- |Computes the offset required to get from the second to the first argument.
(⨪) ∷ P → P → I
(⨪) = minusAddr#

(.//) ∷ P → I → I
(.//) = remAddr#

pattern P# ∷ I → P
pattern P# i ← (addr2Int# → i) where P# = int2Addr#
{-# DEPRECATED P# "This pattern is strongly deprecated" #-}

instance (≤) P where (>) = coerce gtAddr# ; (≥) = coerce geAddr# ; (<) = coerce ltAddr# ; (≤) = coerce leAddr# ; 
instance (≡) P where (≡) = coerce eqAddr# ; (≠) = coerce neAddr#

toAny ∷ P → (# a #)
toAny = addrToAny#

-- | Must be run on an evaluated value, not a thunk
fromAny# ∷ a → IO# P
fromAny# = anyToAddr#
