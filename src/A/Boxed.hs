{-# language MultiParamTypeClasses, FlexibleInstances,TypeSynonymInstances, AllowAmbiguousTypes, TypeFamilies #-}
module A.Boxed where
import A
import A.Prim

type A = Array#
type MA = MutableArray#

class Array (a ∷ T → T_A) where
  read ∷ M (a x) s → I → ST# s x
  write ∷ M (a x) s → I → x → ST_# s
  -- | Read from the specified index of an immutable array.
  -- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
  -- Pattern matching on the tuple forces the indexing of the array to happen
  -- but does not evaluate the element itself. Evaluating the thunk prevents
  -- additional thunks from building up on the heap. Avoiding these thunks, in turn,
  -- reduces references to the argument array, allowing it to be garbage collected more promptly.
  -- Warning: this can fail with an unchecked exception.
  index# ∷ a x → I → (# x #)
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
  cloneM# ∷ M (a x) s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M (a x) s)
  cas ∷ M (a x) s
      → I -- ^ Source offset
      → x -- ^ Expected old value
      → x -- ^ New value
      → ST# s (# B, x #) -- ^ Whether the swap failed, and the actual new value

instance (≡) (MA s x) where
  x ≡ y = coerce do sameMutableArray# x y
  x ≠ y = ((coerce do sameMutableArray# x y) ¬)


-- | @lenM#@ is safe
instance 𝔸 (A x) where
  freeze## = unsafeFreezeArray#
  freeze# = freezeArray#
  thaw## = unsafeThawArray#
  thaw# = thawArray#
  new# n = let e = raise# "A.Boxed.new#: unintialized index" in newArray# n e
  len = sizeofArray# 
  lenM# = sizeofMutableArray# 
  lenM ma = \s → (# s , sizeofMutableArray# ma #)

class Index (x ∷ T_ r) (a ∷ T_ rr) where index ∷ a → I → x
instance Array Array# where
  read = readArray#
  write = writeArray#
  index# = indexArray#
  clone# = cloneArray#
  cloneM# = cloneMutableArray#
  cas as o a0 a1 s0 = case casArray# as o a0 a1 s0 of
    (# s1, failed', a #) → (# s1, (# B# failed', a #) #)
instance Array SmallArray# where
  read = readSmallArray#
  write = writeSmallArray#
  index# = indexSmallArray#
  clone# = cloneSmallArray#
  cloneM# = cloneSmallMutableArray#
  cas as o a0 a1 s0 = case casSmallArray# as o a0 a1 s0 of
    (# s1, failed', a #) → (# s1, (# B# failed', a #) #)

instance Copy (A a) (MA s a) s where copy = copyArray#
instance Copy (MA s a) (MA s a) s where copy = copyMutableArray#

-- | Forces the indexing but not the value. For more laziness use 'A.Small.index#'
instance (x ∷ T) ∈ (A x) where
  new = newArray#
  write#  = writeArray#
  read#  = readArray#
  index# a i = case indexArray# a i of (# a #) → a