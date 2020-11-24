module Array.Boxed where
import qualified A as Byte

type A = Array#
type MA = MutableArray#


new ∷ I → a → ST# s (MA s a)
new = newArray#

eq ∷ MA s a → MA s a → B#
eq = sameMutableArray#

read ∷ MA s a → I → ST# s a
read = readArray#

write ∷ MA s a → I → a → ST_# s
write = writeArray#

-- | Number of elements
size ∷ A a → I
size = sizeofArray#

-- | Read from the specified index of an immutable array.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself. Evaluating the thunk prevents
-- additional thunks from building up on the heap. Avoiding these thunks, in turn,
-- reduces references to the argument array, allowing it to be garbage collected more promptly.
-- Warning: this can fail with an unchecked exception.
index# ∷ A a → I → (# a #)
index# = indexArray#

-- | MAake a mutable array immutable, without copying.
freeze## ∷ MA s a → ST# s (A a)
freeze## = unsafeFreezeArray#

-- | MAake an immutable array mutable, without copying.
thaw## ∷ A a → ST# s (MA s a)
thaw## = unsafeThawArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copy# ∷ A a -- ^ source
      → I -- ^ source offset
      → MA s a -- ^ destination
      → I -- ^ destination offset
      → I -- ^ number of elements to copy
      → ST_# s
copy# = copyArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copyMA# ∷ MA s a -- ^ source
       → I -- ^ source offset
       → MA s a -- ^ destination
       → I -- ^ destination offset
       → I -- ^ number of elements to copy
       → ST_# s
copyMA# = copyMutableArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
clone# ∷ A a
       → I -- ^ Source offset
       → I -- ^ number of elements to copy
       → A a
clone# = cloneArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
cloneMA# ∷ MA s a
        → I -- ^ Source offset
        → I -- ^ number of elements to copy
        → ST# s (MA s a)
cloneMA# = cloneMutableArray#

freeze# ∷ MA s a
        → I -- ^ Source offset
        → I -- ^ number of elements to copy
        → ST# s (A a)
freeze# = freezeArray#

thaw# ∷  A a
        → I -- ^ Source offset
        → I -- ^ number of elements to copy
        → ST# s (MA s a)
thaw# = thawArray#

cas# ∷ MA s a
    → I -- ^ Source offset
    → a -- ^ Expected old value
    → a -- ^ New value
    → ST# s (# B#, a #) -- ^ Whether the swap failed, and the actual new value
cas# as o a0 a1 s0 = case casArray# as o a0 a1 s0 of
  (# s1, failed', a #) → (# s1, (# failed', a #) #)
