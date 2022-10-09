module N where
import GHC.Num.Natural
import GHC.Num.BigNat
import GHC.Types qualified as GHC
import GHC.Types (Bool(..),Word)
import Coerce
import Swap 
import N.Big

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

-- Logarithms
log2' ∷ N → (# (##) | U #)
log2' = naturalIsPowerOf2#

log2 ∷ N → U
log2 = naturalLog2#

logU ∷ U → N → U
logU = naturalLogBaseWord#

log ∷ N → N → U
log = naturalLogBase#

powMod ∷ N {- ^ base -} → N {- ^ exponent -} → N {- ^ modulo -} → N
powMod = naturalPowMod
{-# inline powMod #-}

baseDigits ∷ U {- ^ base -} → N → U {- ^ # of digits in base representation -}
baseDigits = naturalSizeInBase#
{-# inline baseDigits #-}

-- Serialization

-- | Write in base-256 representation and return the
-- number of bytes written.
write256 ∷ N → P s U8 → B# {- ^ big endian? -} → ST s U {- ^ number of bytes written -}
write256 = coerce naturalToAddr#

-- | Read a Natural in base-256 representation from an Addr#.
--
-- Null higher limbs are automatically trimed.
read256 ∷ P## s U8 → B# {- ^ big endian? -} → ST s N
read256 (P_Len# (# p, cast → n #)) = coerce (naturalFromAddr# n p)

instance Cast N Nat where cast = coerce naturalFromBigNat#
instance Cast Nat N where cast = coerce naturalToBigNat#

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
instance Cast F8 (# N, I #) where cast (# m, e #) = naturalEncodeDouble# m e
-- | (# Mantissa, Exponent #)
instance Cast F4 (# N, I #) where cast (# m, e #) = naturalEncodeFloat# m e

-- | Clamp to @maxBound@
toWord# ∷ N → U
toWord# = naturalToWordClamp#

-- | Clamp to @maxBound@
clamp ∷ N → Word 
clamp = naturalToWordClamp

-- | Clamp to @maxBound@
clamp# ∷ N → U
clamp# = naturalToWordClamp#

instance Eq# N where
  (==#) = coerce naturalEq#
  (!=#) = coerce naturalNe#
  (==) = coerce naturalEq
  (!=) = coerce naturalNe

instance Cast Ordering GHC.Ordering where
  cast = \case {GHC.LT → LT; GHC.GT → GT; GHC.EQ → EQ}

instance Cmp# N where
  (<=#) = coerce naturalLe#
  (<#) = coerce naturalLt#
  (>#) = coerce naturalGt#
  (>=#) = coerce naturalGe#
  (<=) = coerce naturalLe
  (<) = coerce naturalLt
  (>) = coerce naturalGt
  (>=) = coerce naturalGe
--  cmp n m = cast (naturalCompare n m)
  cmp (NS x) (NS y) = cmp x y
  cmp (NB x) (NB y) = cast (bigNatCompare x y)
  cmp (NS _) (NB _) = LT
  cmp (NB _) (NS _) = GT
  min x y = if x <= y then x else y
  max x y = if x >= y then x else y

instance Bits N where
  popCnt = naturalPopCount#
  (>>#) = naturalShiftR#
  (>>) = naturalShiftR# -- TODO: check this works
  (<<#) = naturalShiftL#
  (<<) = naturalShiftL# -- TODO: check this works
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit' = coerce naturalTestBit#
  bit = coerce naturalBit#
  ctz = \case {NS u → ctz# u; NB u → ctz @Nat (coerce u)}
  clz = raise# "clz BigNat"
  pdep = pdep
  pext = pext
  byteSwap = byteSwap
  bitReverse = bitReverse -- TODO: remove these

instance Num# N where
  (+) = naturalAdd
  (-) = naturalSubUnsafe
  x -? y = naturalSub x y
  x -?? y = let z' = naturalSub x y in case z' of {(# (##) | #) → (# | x - y #); _ → unsafeCoerce# z'}
  (*) = naturalMul
  (/%) = naturalQuotRem#
  (/) = naturalQuot
  (%) = naturalRem

instance Logic N where 
  (&&) = naturalAnd
  (||) = naturalOr
  not = naturalNot
  xor = naturalXor

naturalNot ∷ N → N; {-# NOINLINE naturalNot #-}
naturalNot = naturalNot -- TODO: put proper error

{-# RULES "and/notR" forall x y. naturalAnd x (naturalNot y) = naturalAndNot x y #-}
{-# RULES "and/notL" forall x y. naturalAnd (naturalNot x) y = naturalAndNot y x #-}


-- TODO: naturalsubthrow
