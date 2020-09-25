module Array.Boxed where
import qualified Array

type A = Array#
type M = MutableArray#


new ∷ I64 → a → ST s (M s a)
new = newArray#

eq ∷ M s a → M s a → B
eq = sameMutableArray#

read ∷ M s a → I64 → ST s a
read = readArray#

write ∷ M s a → I64 → a → ST_ s
write = writeArray#

-- | Number of elements
size ∷ A a → I64
size = sizeofArray#

-- | Read from the specified index of an immutable array.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself. Evaluating the thunk prevents
-- additional thunks from building up on the heap. Avoiding these thunks, in turn,
-- reduces references to the argument array, allowing it to be garbage collected more promptly.
-- Warning: this can fail with an unchecked exception.
index# ∷ A a → I64 → (# a #)
index# = indexArray#

-- | Make a mutable array immutable, without copying.
freeze## ∷ M s a → ST s (A a)
freeze## = unsafeFreezeArray#

-- | Make an immutable array mutable, without copying.
thaw## ∷ A a → ST s (M s a)
thaw## = unsafeThawArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copy# ∷ A a -- ^ source
      → I64 -- ^ source offset
      → M s a -- ^ destination
      → I64 -- ^ destination offset
      → I64 -- ^ number of elements to copy
      → ST_ s
copy# = copyArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copyM# ∷ M s a -- ^ source
       → I64 -- ^ source offset
       → M s a -- ^ destination
       → I64 -- ^ destination offset
       → I64 -- ^ number of elements to copy
       → ST_ s
copyM# = copyMutableArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
clone# ∷ A a
       → I64 -- ^ Source offset
       → I64 -- ^ number of elements to copy
       → A a
clone# = cloneArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
cloneM# ∷ M s a
        → I64 -- ^ Source offset
        → I64 -- ^ number of elements to copy
        → ST s (M s a)
cloneM# = cloneMutableArray#

freeze# ∷ M s a
        → I64 -- ^ Source offset
        → I64 -- ^ number of elements to copy
        → ST s (A a)
freeze# = freezeArray#

thaw# ∷  A a
        → I64 -- ^ Source offset
        → I64 -- ^ number of elements to copy
        → ST s (M s a)
thaw# = thawArray#

cas# ∷ M s a
    → I64 -- ^ Source offset
    → a -- ^ Expected old value
    → a -- ^ New value
    → ST s (# B, a #) -- ^ Whether the swap failed, and the actual new value
cas# as o a0 a1 s0 = case casArray# as o a0 a1 s0 of
  (# s1, failed', a #) → (# s1, (# failed', a #) #)
