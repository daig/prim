module A.Small where
import Prelude hiding (Array)
import A
import A.Prim

type A = SmallArray#
type MA = SmallMutableArray#

(≡) ∷ MA s a → MA s a → B#
(≡) = sameSmallMutableArray#


shrink ∷ MA s a → I → ST_# s
shrink = shrinkSmallMutableArray#

-- | /WARNING/ unsafe in the presence of resize operations
instance Size (A a) where size = sizeofSmallArray#

-- | Number of elements. MAust be in @ST#@ because of possible resizes.
sizeMA# ∷ MA s a → ST# s I
sizeMA# = getSizeofSmallMutableArray#

instance Copy (A a) (MA s a) s where copy = copySmallArray#
instance Copy (MA s a) (MA s a) s where copy = copySmallMutableArray#

instance Write# (a ∷ T) (A a)  where write#  = writeSmallArray#
instance Read# (a ∷ T) (A a)  where read#  = readSmallArray#
-- | Forces the indexing but not the value. For more laziness use 'A.Small.index#'
instance Index# (a ∷ T) (A a) where index# a i = case indexSmallArray# a i of (# a #) → a
