{-# language TypeSynonymInstances, AllowAmbiguousTypes, TypeFamilies #-}
module A.Boxed where
import qualified A as Byte

type A = Array#

(≡) ∷ M Array# s x → M Array# s x → B#
(≡) = sameMutableArray#

type family M (a ∷ T → TYPE UnliftedRep) ∷ T → T → TYPE UnliftedRep where
  M Array# = MutableArray#
  M SmallArray# = SmallMutableArray#

class Array (a ∷ T → TYPE UnliftedRep) where
  new ∷ I → x → ST# s (M a s x)
  read ∷ M a s x → I → ST# s x
  write ∷ M a s x → I → x → ST_# s
  -- | Number of elements
  size ∷ a x → I
  -- | Read from the specified index of an immutable array.
  -- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
  -- Pattern matching on the tuple forces the indexing of the array to happen
  -- but does not evaluate the element itself. Evaluating the thunk prevents
  -- additional thunks from building up on the heap. Avoiding these thunks, in turn,
  -- reduces references to the argument array, allowing it to be garbage collected more promptly.
  -- Warning: this can fail with an unchecked exception.
  index# ∷ a x → I → (# x #)
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s x → ST# s (a x)
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a x → ST# s (M a s x)
  -- | Copy the elements from the source array to the destination array.
  -- Both arrays must fully contain the specified ranges, but this is not checked.
  -- The two arrays must not be the same array in different states, but this is not checked either.
  --
  -- Warning: this can fail with an unchecked exception.
  copy# ∷ a x -- ^ source
        → I -- ^ source offset
        → M a s x -- ^ destination
        → I -- ^ destination offset
        → I -- ^ number of elements to copy
        → ST_# s
  -- | Copy the elements from the source array to the destination array.
  -- Both arrays must fully contain the specified ranges, but this is not checked.
  -- The two arrays must not be the same array in different states, but this is not checked either.
  --
  -- Warning: this can fail with an unchecked exception.
  copyM# ∷ M a s x -- ^ source
         → I -- ^ source offset
         → M a s x -- ^ destination
         → I -- ^ destination offset
         → I -- ^ number of elements to copy
         → ST_# s
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  clone# ∷ a x
         → I -- ^ Source offset
         → I -- ^ number of elements to copy
         → a x
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  cloneM# ∷ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M a s x)
  freeze# ∷ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (a x)
  thaw# ∷  a x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M a s x)
  cas ∷ M a s x
      → I -- ^ Source offset
      → x -- ^ Expected old value
      → x -- ^ New value
      → ST# s (# B#, x #) -- ^ Whether the swap failed, and the actual new value

instance Array Array# where
  new = newArray#
  read = readArray#
  write = writeArray#
  size = sizeofArray#
  index# = indexArray#
  freeze## = unsafeFreezeArray#
  freeze# = freezeArray#
  thaw## = unsafeThawArray#
  thaw# = thawArray#
  copy# = copyArray#
  copyM# = copyMutableArray#
  clone# = cloneArray#
  cloneM# = cloneMutableArray#
  cas as o a0 a1 s0 = case casArray# as o a0 a1 s0 of
    (# s1, failed', a #) → (# s1, (# failed', a #) #)

instance Array SmallArray# where
  new = newSmallArray#
  read = readSmallArray#
  write = writeSmallArray#
  -- | /WARNING/ unsafe in the presence of resize operations
  size = sizeofSmallArray#
  index# = indexSmallArray#
  freeze## = unsafeFreezeSmallArray#
  freeze# = freezeSmallArray#
  thaw## = unsafeThawSmallArray#
  thaw# = thawSmallArray#
  copy# = copySmallArray#
  copyM# = copySmallMutableArray#
  clone# = cloneSmallArray#
  cloneM# = cloneSmallMutableArray#
  cas as o a0 a1 s0 = case casSmallArray# as o a0 a1 s0 of
    (# s1, failed', a #) → (# s1, (# failed', a #) #)
