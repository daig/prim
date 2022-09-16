module N where
import GHC.Num.Natural
import GHC.Num.BigNat
import GHC.Types qualified as GHC
import GHC.Types (Bool(..),Word)

valid'# ∷ N → B#
valid'# = coerce naturalCheck#

valid' ∷ N → Bool
valid' = naturalCheck

zero,one ∷ N
zero = naturalZero
one = naturalOne

zero',one' ∷ N → Bool
zero' = naturalIsZero
one' = naturalIsOne

pattern Z ← (naturalIsZero → True) where Z = naturalZero
pattern One ← (naturalIsOne → True) where One = naturalOne

log2' ∷ N → (# (##) | U #)
log2' = naturalIsPowerOf2#

instance Cast N BigNat# where cast = naturalFromBigNat#
instance Cast BigNat# N where cast = naturalToBigNat#

instance Cast N U where cast = naturalFromWord#
-- | Convert the lower bits
instance Cast U N where cast = naturalToWord#
instance Cast N (# U, U #) where cast (# hi, lo #) = naturalFromWord2# hi lo
-- | Convert the lower bits
instance Cast N Word where cast = naturalFromWord
instance Cast Word N where cast = naturalToWord
instance Cast N [Word] where cast = naturalFromWordList

instance Cast (# (##) | U #) N where cast = naturalToWordMaybe#
-- | (# Mantissa, Exponent #)
instance Cast F64 (# N, I #) where cast (# m, e #) = naturalEncodeDouble# m e
-- | (# Mantissa, Exponent #)
instance Cast F32 (# N, I #) where cast (# m, e #) = naturalEncodeFloat# m e

-- | Clamp to @maxBound@
toWord# ∷ N → U
toWord# = naturalToWordClamp#

-- | Clamp to @maxBound@
toWord ∷ N → Word 
toWord = naturalToWordClamp

instance (≡) N where
  (≡) = coerce naturalEq#
  (≠) = coerce naturalNe#

instance Cast Ordering GHC.Ordering where
  cast = \case {GHC.LT → LT; GHC.GT → GT; GHC.EQ → EQ}

instance (≤) N where
  (≤) = coerce naturalLe#
  (<) = coerce naturalLt#
  (>) = coerce naturalGt#
  (≥) = coerce naturalGe#
--  cmp n m = cast (naturalCompare n m)
  cmp (NS x) (NS y) = cmp x y
  cmp (NB x) (NB y) = cast (bigNatCompare x y)
  cmp (NS _) (NB _) = LT
  cmp (NB _) (NS _) = GT
  min x y = if cast (x ≤ y) then x else y
  max x y = if cast (x ≥ y) then x else y
