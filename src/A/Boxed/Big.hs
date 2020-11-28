--------------------------------------------------------------------
-- | Description : Boxed arrays more efficient for many elements
--------------------------------------------------------------------
{-# language MultiParamTypeClasses, FlexibleInstances,TypeSynonymInstances, AllowAmbiguousTypes, TypeFamilies #-}
module A.Boxed.Big where
import A 

type A = Array#
type MA = MutableArray#
type instance M (A x) s = MA s x

instance (≡) (MA s x) where
  x ≡ y = coerce do sameMutableArray# x y
  x ≠ y = ((coerce do sameMutableArray# x y) ¬)


-- | "A.Boxed" @new#@ initializes undefined. @lenM#@ is safe.
instance 𝔸 (A x) where
  freeze## = unsafeFreezeArray#
  freeze# = freezeArray#
  thaw## = unsafeThawArray#
  thaw# = thawArray#
  new# n = let e = raise# "A.Boxed.new#: unintialized index" in newArray# n e
  len = sizeofArray# 
  lenM# = sizeofMutableArray# 
  lenM ma = \s → (# s , sizeofMutableArray# ma #)
  clone# = cloneArray#
  cloneM# = cloneMutableArray#

  -- | Read from the specified index of an immutable array.
  -- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
  -- Pattern matching on the tuple forces the indexing of the array to happen
  -- but does not evaluate the element itself. Evaluating the thunk prevents
  -- additional thunks from building up on the heap. Avoiding these thunks, in turn,
  -- reduces references to the argument array, allowing it to be garbage collected more promptly.
  -- Warning: this can fail with an unchecked exception.
indexLazy# ∷ A x → I → (# x #)
indexLazy# = indexArray#

-- | Atomic compare and swap
-- (i.e. write the new  value if the current value and
-- the old value are the same pointer).
--  
-- Implies a full memory barrier.
--
-- The use of a pointer equality on a lifted value makes this function harder
-- to use correctly than @casIntArray\#@. All of the difficulties
-- of using 'LiftedRep' '(≡)' correctly apply to @casArray\#@ as well.
cas# ∷ MA s x
     → I -- ^ Source offset
     → x -- ^ Expected old value
     → x -- ^ New value
     → ST# s (Maybe# x) -- ^ Whether the swap failed, and the actual new value
cas# as o a0 a1 s0 = case casArray# as o a0 a1 s0 of
  (# s1, failed', a #) → (# s1, (# B# failed', a #) #)

instance Copy (A a) (MA s a) s where copy = copyArray#
instance Copy (MA s a) (MA s a) s where copy = copyMutableArray#

-- | "A.Boxed.Big"
-- Forces the indexing but not the value. For more laziness use 'indexLazy#'
instance (x ∷ T) ∈ (A x) where
  new = newArray#
  write#  = writeArray#
  read#  = readArray#
  index# a i = case indexArray# a i of (# a #) → a
