module Array.Byte where
import Prelude hiding (Array)

type Array = ByteArray#
type Mutable = MutableByteArray#

-- | Create a new uninitialized mutable byte array of specified size (in bytes),
-- in the specified state thread.
new :: I64 -> ST s (Mutable s)
new = newByteArray#

eq :: Mutable s -> Mutable s -> B
eq = sameMutableByteArray#

shrink :: Mutable s -> I64 -> ST_ s
shrink = shrinkMutableByteArray#

-- | Number of elements
size :: Array -> I64
size = sizeofByteArray#

-- | Number of elements. Must be in @ST@ because of possible resizes.
sizeM :: Mutable s -> ST s I64
sizeM = getSizeofMutableByteArray#

-- | Make a mutable array immutable, without copying.
freeze## :: Mutable s -> ST s (Array)
freeze## = unsafeFreezeByteArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copy# :: Array -- ^ source
      -> I64 -- ^ source offset
      -> Mutable s -- ^ destination
      -> I64 -- ^ destination offset
      -> I64 -- ^ number of elements to copy
      -> ST_ s
copy# = copyByteArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copyM# :: Mutable s -- ^ source
       -> I64 -- ^ source offset
       -> Mutable s -- ^ destination
       -> I64 -- ^ destination offset
       -> I64 -- ^ number of elements to copy
       -> ST_ s
copyM# = copyMutableByteArray#

-- | Copy a range of the @Array@ to the memory range starting at the @Addr@.
-- The @Array@ and the memory region at @Addr@ must fully contain the specified ranges, but this is not checked.
-- The @Addr@ must not point into the @Array@ (e.g. if the @Array@ were pinned), but this is not checked either. 
--
-- Warning: this can fail with an unchecked exception.
copyToAddr# :: Array -- ^ source
            -> I64 -- ^ source offset
            -> Addr -- ^ destination
            -> I64 -- ^ number of elements to copy
            -> ST_ s
copyToAddr# = copyByteArrayToAddr#

-- | Copy a range of the @Array@ to the memory range starting at the @Addr@.
-- The @Array@ and the memory region at @Addr@ must fully contain the specified ranges, but this is not checked.
-- The @Addr@ must not point into the @Array@ (e.g. if the @Array@ were pinned), but this is not checked either. 
--
-- Warning: this can fail with an unchecked exception.
copyToAddrM# :: Mutable s -- ^ source
             -> I64 -- ^ source offset
             -> Addr -- ^ destination
             -> I64 -- ^ number of elements to copy
             -> ST_ s
copyToAddrM# = copyMutableByteArrayToAddr#

-- |Copy a memory range starting at the @Addr@ to the specified range in the
--    @Mutable@. The memory region at @Addr@ and the @Array@ must fully
--    contain the specified ranges, but this is not checked. The @Addr@ must not
--    point into the @Mutable@ (e.g. if the @Mutable@ were pinned),
--    but this is not checked either.
--
--    Warning: This can fail with an unchecked exception.
copyFromAddr# :: Addr -- ^ source
              -> Mutable s -- ^ destination
              -> I64 -- ^ destination offset
              -> I64 -- ^ number of elements to copy
              -> ST_ s
copyFromAddr# = copyAddrToByteArray#

-- | Set a slice to the specified byte.
set :: Mutable s
    -> I64 -- ^ slice start offset
    -> I64 -- ^ slice length in bytes
    -> I64 -- ^ the byte to set them to
    -> ST_ s
set = setByteArray#



-- | Lexicographic comparison.
-- Warning: Both arrays mus fully contain the specified ranges, but this is not checked.
compare# :: Array -- ^ source1
         -> I64 -- ^ source1 offset
         -> Array -- ^ source2
         -> I64 -- ^ source2 offset
         -> I64 -- ^ number of bytes to compare
         -> I64 -- ^ a number less-than, equal-to, or greater-than @0#@
compare# = compareByteArrays#
