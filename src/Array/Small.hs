module Array.Small where
import Prelude hiding (Array)

type A = SmallArray#
type M = SmallMutableArray#

new ∷ I → a → ST# s (M s a)
new = newSmallArray#

(≡), eq ∷ M s a → M s a → B#
(≡) = sameSmallMutableArray#; eq = sameSmallMutableArray#

shrink ∷ M s a → I → ST_# s
shrink = shrinkSmallMutableArray#

read ∷ M s a → I → ST# s a
read = readSmallArray#

write ∷ M s a → I → a → ST_# s
write = writeSmallArray#

-- | Number of elements
size ∷ A a → I
size = sizeofSmallArray#

-- | Number of elements. Must be in @ST#@ because of possible resizes.
sizeM# ∷ M s a → ST# s I
sizeM# = getSizeofSmallMutableArray#

-- | Read from the specified index of an immutable array.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself. Evaluating the thunk prevents
-- additional thunks from building up on the heap. Avoiding these thunks, in turn,
-- reduces references to the argument array, allowing it to be garbage collected more promptly.
-- Warning: this can fail with an unchecked exception.
index# ∷ A a → I → (# a #)
index# = indexSmallArray#

-- | Make a mutable array immutable, without copying.
freeze## ∷ M s a → ST# s (A a)
freeze## = unsafeFreezeSmallArray#

-- | Make an immutable array mutable, without copying.
thaw## ∷ A a → ST# s (M s a)
thaw## = unsafeThawSmallArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copy# ∷ A a -- ^ source
      → I -- ^ source offset
      → M s a -- ^ destination
      → I -- ^ destination offset
      → I -- ^ number of elements to copy
      → ST_# s
copy# = copySmallArray#

-- | Copy the elements from the source array to the destination array.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
--
-- Warning: this can fail with an unchecked exception.
copyM# ∷ M s a -- ^ source
       → I -- ^ source offset
       → M s a -- ^ destination
       → I -- ^ destination offset
       → I -- ^ number of elements to copy
       → ST_# s
copyM# = copySmallMutableArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
clone# ∷ A a
       → I -- ^ Source offset
       → I -- ^ number of elements to copy
       → A a
clone# = cloneSmallArray#

-- | Create a new array with the elements from the source array.
-- The provided array must fully contain the specified range, but this is not checked.
--
-- Warning: this can fail with an unchecked exception.
cloneM# ∷ M s a
        → I -- ^ Source offset
        → I -- ^ number of elements to copy
        → ST# s (M s a)
cloneM# = cloneSmallMutableArray#

freeze# ∷ M s a
        → I -- ^ Source offset
        → I -- ^ number of elements to copy
        → ST# s (A a)
freeze# = freezeSmallArray#

thaw# ∷  A a
        → I -- ^ Source offset
        → I -- ^ number of elements to copy
        → ST# s (M s a)
thaw# = thawSmallArray#

cas ∷ M s a
    → I -- ^ Source offset
    → a -- ^ Expected old value
    → a -- ^ New value
    → ST# s (# B#, a #) -- ^ Whether the swap failed, and the actual new value
cas as o a0 a1 s0 = case casSmallArray# as o a0 a1 s0 of
  (# s1, failed', a #) → (# s1, (# failed', a #) #)
