module A.P where
import A
import Char
import Char8
import P
import qualified P.Stable as Stable

-- | @Raw.P@ are already mutable
type instance M P s = P

-- | "A.P"
instance (♭) a ⇒ (a ∷ T_ r) ∈ P where index# = indexP#; read# = readP#; write# = writeP#
{-
-- | Offset in 4-byte words
instance Char8 ∈ P where
  index# = coerce indexCharOffAddr#
  read#   = coerce readCharOffAddr#
  write#  = coerce writeCharOffAddr#
instance Char ∈ P where
  index# = indexCharOffAddr#
  read#   = readCharOffAddr#
  write#  = writeCharOffAddr#
instance F32 ∈ P where
  index# = indexFloatOffAddr#
  read#   = readFloatOffAddr#
  write#  = writeFloatOffAddr#
instance F64 ∈ P where
  index# = indexDoubleOffAddr#
  read#   = readDoubleOffAddr#
  write#  = writeDoubleOffAddr#
instance I ∈ P where
  index# = indexIntOffAddr#
  read#   = readIntOffAddr#
  write#  = writeIntOffAddr#
instance I8 ∈ P where
  index# = coerce indexInt8OffAddr#
  read#   = coerce readInt8OffAddr#
  write#  = coerce writeInt8OffAddr#
instance I16 ∈ P where
  index# = coerce indexInt16OffAddr#
  read#   = coerce readInt16OffAddr#
  write#  = coerce writeInt16OffAddr#
instance I32 ∈ P where
  index# = coerce indexInt32OffAddr#
  read#   = coerce readInt32OffAddr#
  write#  = coerce writeInt32OffAddr#
instance I64 ∈ P where
  index# = coerce indexInt64OffAddr#
  read#   = coerce readInt64OffAddr#
  write#  = coerce writeInt64OffAddr#
instance U ∈ P where
  index# = indexWordOffAddr#
  read#   = readWordOffAddr#
  write#  = writeWordOffAddr#
instance U8 ∈ P where
  index# = coerce indexWord8OffAddr#
  read#   = coerce readWord8OffAddr#
  write#  = coerce writeWord8OffAddr#
instance U16 ∈ P where
  index# = coerce indexWord16OffAddr#
  read#   = coerce readWord16OffAddr#
  write#  = coerce writeWord16OffAddr#
instance U32 ∈ P where
  index# = coerce indexWord32OffAddr#
  read#   = coerce readWord32OffAddr#
  write#  = coerce writeWord32OffAddr#
instance U64 ∈ P where
  index# = coerce indexWord64OffAddr#
  read#   = coerce readWord64OffAddr#
  write#  = coerce writeWord64OffAddr#
instance P ∈ P where
  index# = indexAddrOffAddr#
  read#   = readAddrOffAddr#
  write#  = writeAddrOffAddr#
instance (Stable.P a) ∈ P where
  index# = indexStablePtrOffAddr#
  read#   = readStablePtrOffAddr#
  write#  = writeStablePtrOffAddr#
  -}
