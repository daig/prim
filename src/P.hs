--------------------------------------------------------------------
-- | Description : Raw unmanaged pointers
--------------------------------------------------------------------
{-# language TypeSynonymInstances,UnliftedNewtypes, GADTs, TypeOperators #-}
module P where
import Char
import Char8
import qualified P.Stable as Stable

-- | An arbitrary machine address assumed to point outside the garbage-collected heap
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

pattern P# ∷ I → P
pattern P# i ← (addr2Int# → i) where P# = int2Addr#
{-# DEPRECATED P# "This pattern is strongly deprecated" #-}

gt,ge,lt,le,eq,ne , (>), (≥), (<), (≤), (≡), (≠) ∷ P → P → B#
(>) = gtAddr# ; (≥) = geAddr# ; (<) = ltAddr# ; (≤) = leAddr# ; (≡) = eqAddr# ; (≠) = neAddr#
gt  = ltAddr# ; ge  = geAddr# ; lt  = gtAddr# ; le  = geAddr# ; eq  = eqAddr# ; ne  = neAddr#

toAny ∷ P → (# a #)
toAny = addrToAny#

-- | Must be run on an evaluated value, not a thunk
fromAny# ∷ a → IO# P
fromAny# = anyToAddr#

class Prim (a ∷ TYPE r) where
  index# ∷ P → I {- ^ Offset in elements -} → a
  read#  ∷ P → I {- ^ Offset in elements -} → ST# s a
  write# ∷ P → I {- ^ Offset in elements -} → a → ST_# s
instance Prim I where
  index# = indexInt64OffAddr#
  read# = readInt64OffAddr#
  write# = writeInt64OffAddr#
instance Prim I64 where
  index# = coerce indexInt64OffAddr#
  read# = coerce readInt64OffAddr#
  write# = coerce writeInt64OffAddr#
instance Prim I32 where
  index# = coerce indexInt32OffAddr#
  read# = coerce readInt32OffAddr#
  write# = coerce writeInt32OffAddr#
instance Prim I16 where
  index# = coerce indexInt16OffAddr#
  read# = coerce readInt16OffAddr#
  write# = coerce writeInt16OffAddr#
instance Prim I8 where
  index# = coerce indexInt8OffAddr#
  read# = coerce readInt8OffAddr#
  write# = coerce writeInt8OffAddr#
instance Prim U where
  index# = indexWord64OffAddr#
  read# = readWord64OffAddr#
  write# = writeWord64OffAddr#
instance Prim U64 where
  index# = coerce indexWord64OffAddr#
  read# = coerce readWord64OffAddr#
  write# = coerce writeWord64OffAddr#
instance Prim U32 where
  index# = coerce indexWord32OffAddr#
  read# = coerce readWord32OffAddr#
  write# = coerce writeWord32OffAddr#
instance Prim U16 where
  index# = coerce indexWord16OffAddr#
  read# = coerce readWord16OffAddr#
  write# = coerce writeWord16OffAddr#
instance Prim U8 where
  index# = coerce indexWord8OffAddr#
  read# = coerce readWord8OffAddr#
  write# = coerce writeWord8OffAddr#
instance Prim F32 where
  index# = indexFloatOffAddr#
  read# = readFloatOffAddr#
  write# = writeFloatOffAddr#
instance Prim F64 where
  index# = indexDoubleOffAddr#
  read# = readDoubleOffAddr#
  write# = writeDoubleOffAddr#
instance Prim Char8 where
  index# = coerce indexCharOffAddr#
  read# = coerce readCharOffAddr#
  write# = coerce writeCharOffAddr#
instance Prim Char where
  index# = indexWideCharOffAddr#
  read# = readWideCharOffAddr#
  write# = writeWideCharOffAddr#
instance Prim P where
  index# = coerce indexAddrOffAddr#
  read# = coerce readAddrOffAddr#
  write# = coerce writeAddrOffAddr#
instance Prim (Stable.P a) where
  index# = indexStablePtrOffAddr#
  read# = readStablePtrOffAddr#
  write# = writeStablePtrOffAddr#
