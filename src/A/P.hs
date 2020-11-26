module A.P where
import A
import Char
import Char8
import P
import qualified P.Stable as Stable

-- | Offset in 4-byte words
instance Char8 ∈ P where
  index# = coerce indexCharOffAddr#
  index## p i = coerce do indexCharOffAddr# (i ∔ p) 0#
  read#   = coerce readCharOffAddr#
  write#  = coerce writeCharOffAddr#
instance Char ∈ P where
  index# = indexCharOffAddr#
  index## p i = indexCharOffAddr# (i ∔ p) 0#
  read#   = readCharOffAddr#
  write#  = writeCharOffAddr#
instance F32 ∈ P where
  index# = indexFloatOffAddr#
  index## p i = indexFloatOffAddr# (i ∔ p) 0#
  read#   = readFloatOffAddr#
  write#  = writeFloatOffAddr#
instance F64 ∈ P where
  index# = indexDoubleOffAddr#
  index## p i = indexDoubleOffAddr# (i ∔ p) 0#
  read#   = readDoubleOffAddr#
  write#  = writeDoubleOffAddr#
instance I ∈ P where
  index# = indexIntOffAddr#
  index## p i = indexIntOffAddr# (i ∔ p) 0#
  read#   = readIntOffAddr#
  write#  = writeIntOffAddr#
instance I8 ∈ P where
  index# = coerce indexInt8OffAddr#
  index## p i = coerce indexInt8OffAddr# (i ∔ p) 0#
  read#   = coerce readInt8OffAddr#
  write#  = coerce writeInt8OffAddr#
instance I16 ∈ P where
  index# = coerce indexInt16OffAddr#
  index## p i = coerce indexInt16OffAddr# (i ∔ p) 0#
  read#   = coerce readInt16OffAddr#
  write#  = coerce writeInt16OffAddr#
instance I32 ∈ P where
  index# = coerce indexInt32OffAddr#
  index## p i = coerce indexInt32OffAddr# (i ∔ p) 0#
  read#   = coerce readInt32OffAddr#
  write#  = coerce writeInt32OffAddr#
instance I64 ∈ P where
  index# = coerce indexInt64OffAddr#
  index## p i = coerce indexInt64OffAddr# (i ∔ p) 0#
  read#   = coerce readInt64OffAddr#
  write#  = coerce writeInt64OffAddr#
instance U ∈ P where
  index# = indexWordOffAddr#
  index## p i = indexWordOffAddr# (i ∔ p) 0#
  read#   = readWordOffAddr#
  write#  = writeWordOffAddr#
instance U8 ∈ P where
  index# = coerce indexWord8OffAddr#
  index## p i = coerce indexWord8OffAddr# (i ∔ p) 0#
  read#   = coerce readWord8OffAddr#
  write#  = coerce writeWord8OffAddr#
instance U16 ∈ P where
  index# = coerce indexWord16OffAddr#
  index## p i = coerce indexWord16OffAddr# (i ∔ p) 0#
  read#   = coerce readWord16OffAddr#
  write#  = coerce writeWord16OffAddr#
instance U32 ∈ P where
  index# = coerce indexWord32OffAddr#
  index## p i = coerce indexWord32OffAddr# (i ∔ p) 0#
  read#   = coerce readWord32OffAddr#
  write#  = coerce writeWord32OffAddr#
instance U64 ∈ P where
  index# = coerce indexWord64OffAddr#
  index## p i = coerce indexWord64OffAddr# (i ∔ p) 0#
  read#   = coerce readWord64OffAddr#
  write#  = coerce writeWord64OffAddr#
instance P ∈ P where
  index# = indexAddrOffAddr#
  index## p i = indexAddrOffAddr# (i ∔ p) 0#
  read#   = readAddrOffAddr#
  write#  = writeAddrOffAddr#
instance (Stable.P a) ∈ P where
  index# = indexStablePtrOffAddr#
  index## p i = indexStablePtrOffAddr# (i ∔ p) 0#
  read#   = readStablePtrOffAddr#
  write#  = writeStablePtrOffAddr#
