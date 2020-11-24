module A.Small where
import Prelude hiding (Array)

type A = SmallArray#
type MA = SmallMutableArray#

(≡) ∷ MA s a → MA s a → B#
(≡) = sameSmallMutableArray#

shrink ∷ MA s a → I → ST_# s
shrink = shrinkSmallMutableArray#

-- | Number of elements. MAust be in @ST#@ because of possible resizes.
sizeMA# ∷ MA s a → ST# s I
sizeMA# = getSizeofSmallMutableArray#
