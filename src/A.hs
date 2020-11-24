{-# language InstanceSigs, TypeSynonymInstances #-}
module A where
import P hiding (Prim)
import Char8
import Char
import I32 (I32(..))
import I16 (I16(..))
import I8 (I8(..))
import I64 (I64(..))
import qualified P.Stable as Stable


type A = ByteArray#
type MA = MutableByteArray#

pinned' ∷ A → B#
pinned' = isByteArrayPinned#

pinnedMA' ∷ MA s → B#
pinnedMA' = isMutableByteArrayPinned#

-- | Create a new uninitialized mutable byte array of specified size (in bytes),
-- in the specified state thread.
new ∷ I → ST# s (MA s)
new = newByteArray#

infix 4 ≡
(≡), eq ∷ MA s → MA s → B#
(≡) = sameMutableByteArray#
eq = sameMutableByteArray#

-- | Must be ≤ current 'sizeMA'
shrink ∷ MA s → I {- ^ # bytes #-} → ST_# s
shrink = shrinkMutableByteArray#

-- | Number of elements
size ∷ A → I
size = sizeofByteArray#

-- | Number of elements.
--
-- note: In @ST#@ because of possible resizes.
sizeMA ∷ MA s → ST# s I
sizeMA = getSizeofMutableByteArray#

-- | MAake a mutable array immutable, without copying.
freeze## ∷ MA s → ST# s A
freeze## = unsafeFreezeByteArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copy# ∷ A -- ^ source
      → I -- ^ source offset
      → MA s -- ^ destination
      → I -- ^ destination offset
      → I -- ^ number of elements to copy
      → ST_# s
copy# = copyByteArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copyMA# ∷ MA s -- ^ source
       → I -- ^ source offset
       → MA s -- ^ destination
       → I -- ^ destination offset
       → I -- ^ number of elements to copy
       → ST_# s
copyMA# = copyMutableByteArray#

-- | Copy a range of the @A@ to the memory range starting at the @P@.
-- The @A@ and the memory region at @P@ must fully contain the specified ranges, but this is not checked.
-- The @P@ must not point into the @A@ (e.g. if the @A@ were pinned), but this is not checked either. 
--
-- Warning: this can fail with an unchecked exception.
copyToP# ∷ A -- ^ source
            → I -- ^ source offset
            → P -- ^ destination
            → I -- ^ number of elements to copy
            → ST_# s
copyToP# = copyByteArrayToAddr#

-- | Copy a range of the @A@ to the memory range starting at the @P@.
-- The @A@ and the memory region at @P@ must fully contain the specified ranges, but this is not checked.
-- The @P@ must not point into the @A@ (e.g. if the @A@ were pinned), but this is not checked either. 
--
-- Warning: this can fail with an unchecked exception.
copyToPMA# ∷ MA s -- ^ source
             → I -- ^ source offset
             → P -- ^ destination
             → I -- ^ number of elements to copy
             → ST_# s
copyToPMA# = copyMutableByteArrayToAddr#

-- |Copy a memory range starting at the @P@ to the specified range in the
--    @Mutable@. The memory region at @P@ and the @A@ must fully
--    contain the specified ranges, but this is not checked. The @P@ must not
--    point into the @Mutable@ (e.g. if the @Mutable@ were pinned),
--    but this is not checked either.
--
--    Warning: This can fail with an unchecked exception.
copyFromP# ∷ P -- ^ source
              → MA s -- ^ destination
              → I -- ^ destination offset
              → I -- ^ number of elements to copy
              → ST_# s
copyFromP# = copyAddrToByteArray#

-- | Set a slice to the specified byte.
set ∷ MA s
    → I -- ^ slice start offset
    → I -- ^ slice length in bytes
    → I -- ^ the byte to set them to
    → ST_# s
set = setByteArray#



-- | Lexicographic comparison.
-- Warning: Both arrays mus fully contain the specified ranges, but this is not checked.
compare# ∷ A -- ^ source1
         → I -- ^ source1 offset
         → A -- ^ source2
         → I -- ^ source2 offset
         → I -- ^ number of bytes to compare
         → I -- ^ a number less-than, equal-to, or greater-than @0#@
compare# = compareByteArrays#

class Prim (a ∷ TYPE r) where
  index# ∷ A → I {- ^ Offset in elements -} → a
  index## ∷ A → I {- ^ Offset in bytes -} → a
  read# ∷ MA s → I → ST# s a
  write# ∷ MA s → I → a → ST_# s
instance Prim Char8 where
  index# ∷ A → I {- ^ Offset in 4-byte words -} → Char8
  index# = coerce indexCharArray#
  index## = coerce indexWord8ArrayAsChar#
  read# = coerce readCharArray#
  write# = coerce writeCharArray#
instance Prim Char where
  index# = indexWideCharArray#
  index## = indexWord8ArrayAsWideChar#
  read# = readWideCharArray#
  write# = writeWideCharArray#
instance Prim F32 where
  index# = indexFloatArray#
  index## = indexWord8ArrayAsFloat#
  read# = readFloatArray#
  write# = writeFloatArray#
instance Prim F64 where
  index# = indexDoubleArray#
  index## = indexWord8ArrayAsDouble#
  read# = readDoubleArray#
  write# = writeDoubleArray#
instance Prim I8 where
  index# = coerce indexInt8Array#
  index## = coerce indexInt8Array#
  read# = coerce readInt8Array#
  write# = coerce writeInt8Array#
instance Prim I16 where
  index# = coerce indexInt16Array#
  index## = coerce indexWord8ArrayAsInt16#
  read# = coerce readInt16Array#
  write# = coerce writeInt16Array#
instance Prim I32 where
  index# = coerce indexInt32Array#
  index## = coerce indexWord8ArrayAsInt32#
  read# = coerce readInt32Array#
  write# = coerce writeInt32Array#
instance Prim I64 where
  index# = coerce indexInt64Array#
  index## = coerce indexWord8ArrayAsInt64#
  read# = coerce readInt64Array#
  write# = coerce writeInt64Array#
instance Prim I where
  index# = coerce indexIntArray#
  index## = coerce indexWord8ArrayAsInt#
  read# = coerce readIntArray#
  write# = coerce writeIntArray#
instance Prim U8 where
  index# = coerce indexWord8Array#
  index## = coerce indexWord8Array#
  read# = coerce readWord8Array#
  write# = coerce writeWord8Array#
instance Prim U16 where
  index# = coerce indexWord16Array#
  index## = coerce indexWord8ArrayAsWord16#
  read# = coerce readWord16Array#
  write# = coerce writeWord16Array#
instance Prim U32 where
  index# = coerce indexWord32Array#
  index## = coerce indexWord8ArrayAsWord32#
  read# = coerce readWord32Array#
  write# = coerce writeWord32Array#
instance Prim U64 where
  index# = coerce indexWord64Array#
  index## = coerce indexWord8ArrayAsWord64#
  read# = coerce readWord64Array#
  write# = coerce writeWord64Array#
instance Prim U where
  index# = coerce indexWordArray#
  index## = coerce indexWord8ArrayAsWord#
  read# = coerce readWordArray#
  write# = coerce writeWordArray#
instance Prim P where
  index# = coerce indexAddrArray#
  index## = coerce indexWord8ArrayAsAddr#
  read# = coerce readAddrArray#
  write# = coerce writeAddrArray#
instance Prim (Stable.P a) where
  index# = indexStablePtrArray#
  index## = indexWord8ArrayAsStablePtr#
  read# = readStablePtrArray#
  write# = writeStablePtrArray#
