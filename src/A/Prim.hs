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


{-

instance Index#  ArrayArray# ArrayArray# where index#  = indexArrayArrayArray#
instance Index## ArrayArray# ArrayArray# where index## = indexArrayArrayArray#
instance Read#   ArrayArray# ArrayArray# where read#   = readArrayArrayArray#
instance Write#  ArrayArray# ArrayArray# where write#  = writeArrayArrayArray#
instance Index#  ByteArray#  ArrayArray# where index#  = indexByteArrayArray#
instance Index## ByteArray#  ArrayArray# where index## = indexByteArrayArray#
instance Read#   ByteArray#  ArrayArray# where read#   = readByteArrayArray#
instance Write#  ByteArray#  ArrayArray# where write#  = writeByteArrayArray#


{-
instance Prim (Stable.P a) where
  index# = indexStablePtrArray#
  index## = indexWord8ArrayAsStablePtr#
  read# = readStablePtrArray#
  write# = writeStablePtrArray#
  -}
  -}
