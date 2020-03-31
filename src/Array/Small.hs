module Array.Small where
import Prelude hiding (Array)

type Array = SmallArray#
type Mutable = SmallMutableArray#

new :: I64 -> a -> ST s (Mutable s a)
new = newSmallArray#

eq :: Mutable s a -> Mutable s a -> B
eq = sameSmallMutableArray#

shrink :: Mutable s a -> I64 -> ST_ s
shrink = shrinkSmallMutableArray#

read :: Mutable s a -> I64 -> ST s a
read = readSmallArray#

write :: Mutable s a -> I64 -> a -> ST_ s
write = writeSmallArray#

-- | Number of elements
size :: Array a -> I64
size = sizeofSmallArray#

-- | Number of elements. Must be in @ST@ because of possible resizes.
sizeM# :: Mutable s a -> ST s I64
sizeM# = getSizeofSmallMutableArray#

-- | Read from the specified index of an immutable array.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself. Evaluating the thunk prevents
-- additional thunks from building up on the heap. Avoiding these thunks, in turn,
-- reduces references to the argument array, allowing it to be garbage collected more promptly.
-- Warning: this can fail with an unchecked exception.
index# :: Array a -> I64 -> (# a #)
index# = indexSmallArray#

-- | Make a mutable array immutable, without copying.
freeze## :: Mutable s a -> ST s (Array a)
freeze## = unsafeFreezeSmallArray#

-- | Make an immutable array mutable, without copying.
thaw## :: Array a -> ST s (Mutable s a)
thaw## = unsafeThawSmallArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copy# :: Array a -- ^ source
      -> I64 -- ^ source offset
      -> Mutable s a -- ^ destination
      -> I64 -- ^ destination offset
      -> I64 -- ^ number of elements to copy
      -> ST_ s
copy# = copySmallArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copyM# :: Mutable s a -- ^ source
       -> I64 -- ^ source offset
       -> Mutable s a -- ^ destination
       -> I64 -- ^ destination offset
       -> I64 -- ^ number of elements to copy
       -> ST_ s
copyM# = copySmallMutableArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
clone# :: Array a
       -> I64 -- ^ Source offset
       -> I64 -- ^ number of elements to copy
       -> Array a
clone# = cloneSmallArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
cloneM# :: Mutable s a
        -> I64 -- ^ Source offset
        -> I64 -- ^ number of elements to copy
        -> ST s (Mutable s a)
cloneM# = cloneSmallMutableArray#

freeze# :: Mutable s a
        -> I64 -- ^ Source offset
        -> I64 -- ^ number of elements to copy
        -> ST s (Array a)
freeze# = freezeSmallArray#

thaw# ::  Array a
        -> I64 -- ^ Source offset
        -> I64 -- ^ number of elements to copy
        -> ST s (Mutable s a)
thaw# = thawSmallArray#

cas :: Mutable s a
    -> I64 -- ^ Source offset
    -> a -- ^ Expected old value
    -> a -- ^ New value
    -> ST s (# B, a #) -- ^ Whether the swap failed, and the actual new value
cas as o a0 a1 s0 = case casSmallArray# as o a0 a1 s0 of
  (# s1, failed', a #) -> (# s1, (# failed', a #) #)
