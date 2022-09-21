{-# language FlexibleInstances #-}
module N.Big where
import GHC.Num.BigNat
import GHC.Types as GHC

valid'# ∷ Nat → B#
valid'# = coerce bigNatCheck#

valid' ∷ Nat → Bool
valid' = bigNatCheck

-- | Number of words in the big nat
size ∷ Nat → Word
size = bigNatSize

-- | Number of words in the big nat
size# ∷ Nat → I
size# = bigNatSize#

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
zero# = bigNatZero#
one# = bigNatOne#

zero'#,one'#, two'# ∷ Nat → B#
zero'# = coerce bigNatIsZero#
one'# = coerce bigNatIsOne#
two'# = coerce bigNatIsTwo#

pattern Z# ← (zero'# → T#) where Z# = bigNatZero# (##)
pattern One# ← (one'# → T#) where One# = bigNatOne# (##)

log2' ∷ Nat → (# (##) | U #)
log2' = bigNatIsPowerOf2#

index# ∷ Nat → I → Word
index# = bigNatIndex

instance Cast Nat Word where cast = bigNatFromWord
instance Cast Nat U where cast = bigNatFromWord#
-- | Most significant first. Must be nonzero
instance Cast Nat [Word] where cast = bigNatFromWordList#
-- | Absolute value
instance Cast Nat I where cast = bigNatFromAbsInt#

-- | Like 'cast' but doesn't remove leading zeroes.
fromWordList ∷ [Word] → Nat
fromWordList = bigNatFromWordListUnsafe

instance Cast [Word] Nat where cast = bigNatToWordList
instance Cast Nat (# U, U #) where cast (# hi, lo #) = bigNatFromWord2# hi lo
instance Cast U Nat where cast = bigNatToWord#
instance Cast (# (##) | U #) Nat where cast = bigNatToWordMaybe#
instance Cast Word Nat where cast = bigNatToWord
instance Cast I Nat where cast = bigNatToInt#
instance Cast Int Nat where cast = bigNatToInt
instance Cast Nat U64 where cast = bigNatFromWord64#
instance Cast U64 Nat where cast = bigNatToWord64#
-- | (# Mantissa, Exponent #)
instance Cast F64 (# Nat , I #) where cast (# m, e #) = bigNatEncodeDouble# m e

gtU,eqU,leU ∷ Nat → U → B#
gtU = coerce bigNatGtWord#
leU = coerce bigNatLeWord#
eqU = coerce bigNatEqWord#

gtWord,leWord ∷ Nat → Word → Bool
gtWord = bigNatGtWord
leWord = bigNatLeWord

cmpU ∷ Nat → U → GHC.Ordering
cmpU = bigNatCompareWord#

instance (≡) Nat where
  (≡) = coerce bigNatEq#
  (≠) = coerce bigNatNe#

instance (≤) Nat where
  (<) = coerce bigNatLt#
  (≤) = coerce bigNatLe#
  (>) = coerce bigNatGt#
  (≥) = coerce bigNatGe#
  cmp a b = Ordering# do bigNatGt# a b -# bigNatLt# a b
  min x y = if cast (x ≤ y) then x else y
  max x y = if cast (x ≥ y) then x else y

instance Nat ∔ U where
{-
addU,mulU ∷ Nat → U → Nat
addU# = bigNatAddWord#
mulU = bigNatAdd#


-}
