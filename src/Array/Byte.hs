module Array.Byte where
import qualified Array
import qualified Ref

type A = Array.Byte
type M = MutableByteArray#

-- | Create a new uninitialized mutable byte array of specified size (in bytes),
-- in the specified state thread.
new :: I64 -> ST s (M s)
new = newByteArray#

eq :: M s -> M s -> B
eq = sameMutableByteArray#

shrink :: M s -> I64 -> ST_ s
shrink = shrinkMutableByteArray#

-- | Number of elements
size :: A -> I64
size = sizeofByteArray#

-- | Number of elements. Must be in @ST@ because of possible resizes.
sizeM :: M s -> ST s I64
sizeM = getSizeofMutableByteArray#

-- | Make a mutable array immutable, without copying.
freeze## :: M s -> ST s (A)
freeze## = unsafeFreezeByteArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copy# :: A -- ^ source
      -> I64 -- ^ source offset
      -> M s -- ^ destination
      -> I64 -- ^ destination offset
      -> I64 -- ^ number of elements to copy
      -> ST_ s
copy# = copyByteArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copyM# :: M s -- ^ source
       -> I64 -- ^ source offset
       -> M s -- ^ destination
       -> I64 -- ^ destination offset
       -> I64 -- ^ number of elements to copy
       -> ST_ s
copyM# = copyMutableByteArray#

-- | Copy a range of the @A@ to the memory range starting at the @Ref.Byte@.
-- The @A@ and the memory region at @Ref.Byte@ must fully contain the specified ranges, but this is not checked.
-- The @Ref.Byte@ must not point into the @A@ (e.g. if the @A@ were pinned), but this is not checked either. 
--
-- Warning: this can fail with an unchecked exception.
copyToRef# :: A -- ^ source
            -> I64 -- ^ source offset
            -> Ref.Byte -- ^ destination
            -> I64 -- ^ number of elements to copy
            -> ST_ s
copyToRef# = copyByteArrayToAddr#

-- | Copy a range of the @A@ to the memory range starting at the @Ref.Byte@.
-- The @A@ and the memory region at @Ref.Byte@ must fully contain the specified ranges, but this is not checked.
-- The @Ref.Byte@ must not point into the @A@ (e.g. if the @A@ were pinned), but this is not checked either. 
--
-- Warning: this can fail with an unchecked exception.
copyToRefM# :: M s -- ^ source
             -> I64 -- ^ source offset
             -> Ref.Byte -- ^ destination
             -> I64 -- ^ number of elements to copy
             -> ST_ s
copyToRefM# = copyMutableByteArrayToAddr#

-- |Copy a memory range starting at the @Ref.Byte@ to the specified range in the
--    @Mutable@. The memory region at @Ref.Byte@ and the @A@ must fully
--    contain the specified ranges, but this is not checked. The @Ref.Byte@ must not
--    point into the @Mutable@ (e.g. if the @Mutable@ were pinned),
--    but this is not checked either.
--
--    Warning: This can fail with an unchecked exception.
copyFromRef# :: Ref.Byte -- ^ source
              -> M s -- ^ destination
              -> I64 -- ^ destination offset
              -> I64 -- ^ number of elements to copy
              -> ST_ s
copyFromRef# = copyAddrToByteArray#

-- | Set a slice to the specified byte.
set :: M s
    -> I64 -- ^ slice start offset
    -> I64 -- ^ slice length in bytes
    -> I64 -- ^ the byte to set them to
    -> ST_ s
set = setByteArray#



-- | Lexicographic comparison.
-- Warning: Both arrays mus fully contain the specified ranges, but this is not checked.
compare# :: A -- ^ source1
         -> I64 -- ^ source1 offset
         -> A -- ^ source2
         -> I64 -- ^ source2 offset
         -> I64 -- ^ number of bytes to compare
         -> I64 -- ^ a number less-than, equal-to, or greater-than @0#@
compare# = compareByteArrays#
