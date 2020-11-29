--------------------------------------------------------------------
-- | Description : Small Boxed Arrays
--------------------------------------------------------------------
module A.Boxed where
import Prelude hiding (Array)
import A

type A = SmallArray#
type MA = SmallMutableArray#

-- | "A.Boxed" - @new#@ initializes undefined.
instance 𝔸 (A x) where
  freeze## = unsafeFreezeSmallArray#
  freeze# = freezeSmallArray#
  thaw## = unsafeThawSmallArray#
  thaw# = thawSmallArray#
  new# n = newSmallArray# n (let x = x in x)
  len = sizeofSmallArray#
  lenM# = sizeofSmallMutableArray#
  lenM = getSizeofSmallMutableArray#
  clone# = cloneSmallArray#
  cloneM# = cloneSmallMutableArray#


-- | Read from the specified index of an immutable array.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself. Evaluating the thunk prevents
-- additional thunks from building up on the heap. Avoiding these thunks, in turn,
-- reduces references to the argument array, allowing it to be garbage collected more promptly.
-- Warning: this can fail with an unchecked exception.
indexLazy# ∷ A x → I → (# x #)
indexLazy# = indexSmallArray#

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
cas# as o a0 a1 s0 = case casSmallArray# as o a0 a1 s0 of
  (# s1, failed', a #) → (# s1, (# B# failed', a #) #)

instance (≡) (MA s a) where
  x ≡ y= coerce do sameSmallMutableArray# x y
  x ≠ y = (¬) (x ≡ y)

instance Shrink (A a) where shrink = shrinkSmallMutableArray#


-- | Number of elements. MAust be in @ST#@ because of possible resizes.
sizeMA# ∷ MA s a → ST# s I
sizeMA# = getSizeofSmallMutableArray#

-- | A.Small
instance Copy (A a) (MA s a) s where copy = copySmallArray#
-- | A.Small
instance Copy (MA s a) (MA s a) s where copy = copySmallMutableArray#

-- | "A.Boxed".
--
-- @index#@Forces the indexing but not the value. For more laziness use 'indexLazy#'
--
-- @new@ uses sharing
instance (x ∷ T) ∈ (A x) where
  new = newSmallArray#
  write#  = writeSmallArray#
  read#  = readSmallArray#
  index# a i = case indexSmallArray# a i of (# a #) → a
