{-# language FlexibleInstances #-}
module N.Big where
import GHC.Num.BigNat
import GHC.Types as GHC

valid'# ∷ Nat → B#
valid'# = coerce bigNatCheck#

valid' ∷ Nat → Bool
valid' = coerce bigNatCheck

-- | Number of words in the big nat
size ∷ Nat → Word
size = coerce bigNatSize

-- | Number of words in the big nat
size# ∷ Nat → I
size# = coerce bigNatSize#

zero,one ∷ BigNat
zero = bigNatZero
one = bigNatOne

zero',one', two' ∷ BigNat → Bool
zero' (BN# x) = bigNatIsZero x
one' (BN# x) = bigNatIsOne x
two' (BN# x) = bigNatIsTwo x

pattern Z ← (zero' → True) where Z = bigNatZero
pattern One ← (one' → True) where One = bigNatOne

zero#,one# ∷ (##) → Nat
zero# = coerce bigNatZero#
one# = coerce bigNatOne#

zero'#,one'#, two'# ∷ Nat → B#
zero'# = coerce bigNatIsZero#
one'# = coerce bigNatIsOne#
two'# = coerce bigNatIsTwo#

pattern Z# ← (zero'# → T#) where Z# = coerce bigNatZero# (##)
pattern One# ← (one'# → T#) where One# = coerce bigNatOne# (##)

log2' ∷ Nat → (# (##) | U #)
log2' = coerce bigNatIsPowerOf2#

log2 ∷ Nat → U
log2 = coerce bigNatLog2#

index# ∷ Nat → I → Word
index# = coerce bigNatIndex

index## ∷ Nat → I → U
index## = coerce bigNatIndex#

instance Cast Nat Word where cast = coerce bigNatFromWord
instance Cast Nat U where cast = coerce bigNatFromWord#
-- | Most significant first. Must be nonzero
instance Cast Nat [Word] where cast = coerce bigNatFromWordList#
-- | Absolute value
instance Cast Nat I where cast = coerce bigNatFromAbsInt#

-- | Like 'cast' but doesn't remove leading zeroes.
fromWordList ∷ [Word] → Nat
fromWordList = coerce bigNatFromWordListUnsafe

instance Cast [Word] Nat where cast = coerce bigNatToWordList
instance Cast Nat (# U, U #) where cast (# hi, lo #) = coerce bigNatFromWord2# hi lo
instance Cast U Nat where cast = coerce bigNatToWord#
instance Cast (# (##) | U #) Nat where cast = coerce bigNatToWordMaybe#
instance Cast Word Nat where cast = coerce bigNatToWord
instance Cast I Nat where cast = coerce bigNatToInt#
instance Cast Int Nat where cast = coerce bigNatToInt
instance Cast Nat U64 where cast = coerce bigNatFromWord64#
instance Cast U64 Nat where cast = coerce bigNatToWord64#
-- | (# Mantissa, Exponent #)
instance Cast F64 (# Nat , I #) where cast (# m, e #) = coerce bigNatEncodeDouble# m e

gtU,eqU,leU ∷ Nat → U → B#
gtU = coerce bigNatGtWord#
leU = coerce bigNatLeWord#
eqU = coerce bigNatEqWord#

gtWord,leWord ∷ Nat → Box U → Bool
gtWord = coerce bigNatGtWord
leWord = coerce bigNatLeWord

cmpU ∷ Nat → U → GHC.Ordering
cmpU = coerce bigNatCompareWord#

instance (≡) Nat where
  (≡) = coerce bigNatEq
  (≠) = coerce bigNatNe
  (=#) = coerce bigNatEq#
  (≠#) = coerce bigNatNe#

instance (≤) Nat where
  (<#) = coerce bigNatLt#
  (≤#) = coerce bigNatLe#
  (>#) = coerce bigNatGt#
  (≥#) = coerce bigNatGe#
  (>) = cast ((>#) @Nat)
  (≥) = cast ((≥#) @Nat)
  (<) = cast ((<#) @Nat)
  (≤) = cast ((≤#) @Nat)
  -- | Compare two BigNat
  cmp a b = Ordering# (dataToTag# @GHC.Ordering (coerce bigNatCompare a b) - 1#)
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance Bits Nat where
  (>>#) = coerce bigNatShiftR#
  (<<#) = coerce bigNatShiftL#
  (>>) = (>>#)
  (<<) = (<<#)
  shift w i = if i ≥ 0# then w << cast i else w >> cast (negateInt# i)
  bit' = coerce bigNatTestBit#
  bit = coerce bigNatBit#
  popCnt = coerce bigNatPopCount#
  ctz = coerce bigNatCtz#
  clz = raise# "clz BigNat"
  pdep = pdep
  pext = pext
  byteSwap = byteSwap
  bitReverse = bitReverse -- TODO: remove these

  


--instance Nat +. U where
{-
addU,mulU ∷ Nat → U → Nat
addU# = bigNatAddWord#
mulU = bigNatAdd#


-}
