--------------------------------------------------------------------
-- | Description : Small Boxed Arrays
--------------------------------------------------------------------
module A.Small where
import Prelude hiding (Array)
import A
import A.Prim

type A = SmallArray#
type MA = SmallMutableArray#

instance 𝔸 (A x) where
  freeze## = unsafeFreezeSmallArray#
  freeze# = freezeSmallArray#
  thaw## = unsafeThawSmallArray#
  thaw# = thawSmallArray#
  new# n = newSmallArray# n (let x = x in x)
  len = sizeofSmallArray#
  lenM# = sizeofSmallMutableArray#
  lenM = getSizeofSmallMutableArray#

instance (≡) (MA s a) where
  x ≡ y= coerce do sameSmallMutableArray# x y
  x ≠ y = (¬) (x ≡ y)

instance Shrink (A a) where shrink = shrinkSmallMutableArray#


-- | Number of elements. MAust be in @ST#@ because of possible resizes.
sizeMA# ∷ MA s a → ST# s I
sizeMA# = getSizeofSmallMutableArray#

instance Copy (A a) (MA s a) s where copy = copySmallArray#
instance Copy (MA s a) (MA s a) s where copy = copySmallMutableArray#

-- | Forces the indexing but not the value. For more laziness use 'A.Small.index#'
instance (x ∷ T) ∈ (A x) where
  write#  = writeSmallArray#
  read#  = readSmallArray#
  index# a i = case indexSmallArray# a i of (# a #) → a
