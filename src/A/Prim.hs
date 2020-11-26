{-# language ScopedTypeVariables,FunctionalDependencies, InstanceSigs #-}
module A.Prim where
import A (M)
import Char8 (Char8(..))
import Char (Char(..))
import I8 (I8(..))
import I16 (I16(..))
import I32 (I32(..))
import I64 (I64(..))
import P (P)
import qualified P.Stable as Stable

class (x ∷ T_ r) ∈ (a ∷ T_ r') where
  index# ∷ a → I {- ^ Offset in elements -} → x
  index## ∷ a → I {- ^ Offset in bytes -} → x
  read# ∷ M a s → I → ST# s x
  write# ∷ M a s → I → x → ST_# s

{-
-- | Offset in 4-byte words
instance Index#  Char8       ByteArray#  where index# = coerce indexCharArray#
instance Index## Char8       ByteArray#  where index## = coerce indexWord8ArrayAsChar#
instance Read#   Char8       ByteArray#  where read#   = coerce readCharArray#
instance Write#  Char8       ByteArray#  where write#  = coerce writeCharArray#
instance Index#  Char        ByteArray#  where index# = indexCharArray#
instance Index## Char        ByteArray#  where index## = indexWord8ArrayAsChar#
instance Read#   Char        ByteArray#  where read#   = readCharArray#
instance Write#  Char        ByteArray#  where write#  = writeCharArray#
instance Index#  F32        ByteArray#  where index# = indexFloatArray#
instance Index## F32        ByteArray#  where index## = indexWord8ArrayAsFloat#
instance Read#   F32        ByteArray#  where read#   = readFloatArray#
instance Write#  F32        ByteArray#  where write#  = writeFloatArray#
instance Index#  F64        ByteArray#  where index# = indexDoubleArray#
instance Index## F64        ByteArray#  where index## = indexWord8ArrayAsDouble#
instance Read#   F64        ByteArray#  where read#   = readDoubleArray#
instance Write#  F64        ByteArray#  where write#  = writeDoubleArray#
instance Index#  I8         ByteArray#  where index# = coerce indexInt8Array#
instance Read#   I8         ByteArray#  where read#   = coerce readInt8Array#
instance Write#  I8         ByteArray#  where write#  = coerce writeInt8Array#
instance Index#  I16        ByteArray#  where index# = coerce indexInt16Array#
instance Index## I16        ByteArray#  where index## = coerce indexWord8ArrayAsInt16#
instance Read#   I16        ByteArray#  where read#   = coerce readInt16Array#
instance Write#  I16        ByteArray#  where write#  = coerce writeInt16Array#
instance Index#  I32        ByteArray#  where index# = coerce indexInt32Array#
instance Index## I32        ByteArray#  where index## = coerce indexWord8ArrayAsInt32#
instance Read#   I32        ByteArray#  where read#   = coerce readInt32Array#
instance Write#  I32        ByteArray#  where write#  = coerce writeInt32Array#
instance Index#  I64        ByteArray#  where index# = coerce indexInt64Array#
instance Index## I64        ByteArray#  where index## = coerce indexWord8ArrayAsInt64#
instance Read#   I64        ByteArray#  where read#   = coerce readInt64Array#
instance Write#  I64        ByteArray#  where write#  = coerce writeInt64Array#
instance Index#  I        ByteArray#  where index# = indexIntArray#
instance Index## I        ByteArray#  where index## = indexWord8ArrayAsInt#
instance Read#   I        ByteArray#  where read#   = readIntArray#
instance Write#  I        ByteArray#  where write#  = writeIntArray#
instance Index#  U8         ByteArray#  where index# = coerce indexWord8Array#
instance Read#   U8         ByteArray#  where read#   = coerce readWord8Array#
instance Write#  U8         ByteArray#  where write#  = coerce writeWord8Array#
instance Index#  U16        ByteArray#  where index# = coerce indexWord16Array#
instance Index## U16        ByteArray#  where index## = coerce indexWord8ArrayAsWord16#
instance Read#   U16        ByteArray#  where read#   = coerce readWord16Array#
instance Write#  U16        ByteArray#  where write#  = coerce writeWord16Array#
instance Index#  U32        ByteArray#  where index# = coerce indexWord32Array#
instance Index## U32        ByteArray#  where index## = coerce indexWord8ArrayAsWord32#
instance Read#   U32        ByteArray#  where read#   = coerce readWord32Array#
instance Write#  U32        ByteArray#  where write#  = coerce writeWord32Array#
instance Index#  U64        ByteArray#  where index# = coerce indexWord64Array#
instance Index## U64        ByteArray#  where index## = coerce indexWord8ArrayAsWord64#
instance Read#   U64        ByteArray#  where read#   = coerce readWord64Array#
instance Write#  U64        ByteArray#  where write#  = coerce writeWord64Array#
instance Index#  U        ByteArray#  where index# = indexWordArray#
instance Index## U        ByteArray#  where index## = indexWord8ArrayAsWord#
instance Read#   U        ByteArray#  where read#   = readWordArray#
instance Write#  U        ByteArray#  where write#  = writeWordArray#
instance Index#  P        ByteArray#  where index# = indexAddrArray#
instance Index## P        ByteArray#  where index## = indexWord8ArrayAsAddr#
instance Read#   P        ByteArray#  where read#   = readAddrArray#
instance Write#  P        ByteArray#  where write#  = writeAddrArray#
instance Index#  (Stable.P a) ByteArray#  where index# = indexStablePtrArray#
instance Index## (Stable.P a) ByteArray#  where index## = indexWord8ArrayAsStablePtr#
instance Read#   (Stable.P a) ByteArray#  where read#   = readStablePtrArray#
instance Write#  (Stable.P a) ByteArray#  where write#  = writeStablePtrArray#

instance Index#  ArrayArray# ArrayArray# where index#  = indexArrayArrayArray#
instance Index## ArrayArray# ArrayArray# where index## = indexArrayArrayArray#
instance Read#   ArrayArray# ArrayArray# where read#   = readArrayArrayArray#
instance Write#  ArrayArray# ArrayArray# where write#  = writeArrayArrayArray#
instance Index#  ByteArray#  ArrayArray# where index#  = indexByteArrayArray#
instance Index## ByteArray#  ArrayArray# where index## = indexByteArrayArray#
instance Read#   ByteArray#  ArrayArray# where read#   = readByteArrayArray#
instance Write#  ByteArray#  ArrayArray# where write#  = writeByteArrayArray#

-- | Offset in 4-byte words
instance Index#  Char8       P  where index# = coerce indexCharOffAddr#
instance Read#   Char8       P  where read#   = coerce readCharOffAddr#
instance Write#  Char8       P  where write#  = coerce writeCharOffAddr#
instance Index#  Char        P  where index# = indexCharOffAddr#
instance Read#   Char        P  where read#   = readCharOffAddr#
instance Write#  Char        P  where write#  = writeCharOffAddr#
instance Index#  F32        P  where index# = indexFloatOffAddr#
instance Read#   F32        P  where read#   = readFloatOffAddr#
instance Write#  F32        P  where write#  = writeFloatOffAddr#
instance Index#  F64        P  where index# = indexDoubleOffAddr#
instance Read#   F64        P  where read#   = readDoubleOffAddr#
instance Write#  F64        P  where write#  = writeDoubleOffAddr#
instance Index#  I8         P  where index# = coerce indexInt8OffAddr#
instance Read#   I8         P  where read#   = coerce readInt8OffAddr#
instance Write#  I8         P  where write#  = coerce writeInt8OffAddr#
instance Index#  I16        P  where index# = coerce indexInt16OffAddr#
instance Read#   I16        P  where read#   = coerce readInt16OffAddr#
instance Write#  I16        P  where write#  = coerce writeInt16OffAddr#
instance Index#  I32        P  where index# = coerce indexInt32OffAddr#
instance Read#   I32        P  where read#   = coerce readInt32OffAddr#
instance Write#  I32        P  where write#  = coerce writeInt32OffAddr#
instance Index#  I64        P  where index# = coerce indexInt64OffAddr#
instance Read#   I64        P  where read#   = coerce readInt64OffAddr#
instance Write#  I64        P  where write#  = coerce writeInt64OffAddr#
instance Index#  I        P  where index# = indexIntOffAddr#
instance Read#   I        P  where read#   = readIntOffAddr#
instance Write#  I        P  where write#  = writeIntOffAddr#
instance Index#  U8         P  where index# = coerce indexWord8OffAddr#
instance Read#   U8         P  where read#   = coerce readWord8OffAddr#
instance Write#  U8         P  where write#  = coerce writeWord8OffAddr#
instance Index#  U16        P  where index# = coerce indexWord16OffAddr#
instance Read#   U16        P  where read#   = coerce readWord16OffAddr#
instance Write#  U16        P  where write#  = coerce writeWord16OffAddr#
instance Index#  U32        P  where index# = coerce indexWord32OffAddr#
instance Read#   U32        P  where read#   = coerce readWord32OffAddr#
instance Write#  U32        P  where write#  = coerce writeWord32OffAddr#
instance Index#  U64        P  where index# = coerce indexWord64OffAddr#
instance Read#   U64        P  where read#   = coerce readWord64OffAddr#
instance Write#  U64        P  where write#  = coerce writeWord64OffAddr#
instance Index#  U        P  where index# = indexWordOffAddr#
instance Read#   U        P  where read#   = readWordOffAddr#
instance Write#  U        P  where write#  = writeWordOffAddr#
instance Index#  P        P  where index# = indexAddrOffAddr#
instance Read#   P        P  where read#   = readAddrOffAddr#
instance Write#  P        P  where write#  = writeAddrOffAddr#
instance Index#  (Stable.P a) P  where index# = indexStablePtrOffAddr#
instance Read#   (Stable.P a) P  where read#   = readStablePtrOffAddr#
instance Write#  (Stable.P a) P  where write#  = writeStablePtrOffAddr#

{-
instance Prim (Stable.P a) where
  index# = indexStablePtrArray#
  index## = indexWord8ArrayAsStablePtr#
  read# = readStablePtrArray#
  write# = writeStablePtrArray#
  -}
  -}
