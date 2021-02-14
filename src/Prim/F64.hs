module Prim.F64 (F64 ,module Prim.F64) where
import HsFFI hiding (Inf,Inf_)
import Stock.Double
import Stock.Eq
import Stock.Bool

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
type F64 = Double#

instance (≡) F64 where
  (≡) = coerce (==##)
  (≠) = coerce (/=##)
instance (≤) F64 where
  (>) = coerce (>##)
  (≥) = coerce (>=##)
  (<) = coerce (<##)
  (≤) = coerce (<=##)
instance ℕ F64 where
  (+) = (+##); (×) = (*##)
  (/) = (/##); _ % _ = 0.0##
  x /% y = (# x / y , 0.0## #)
  addC a b = case a + b of {c@Inf  → (# Inf , T #); c → (# c , F #)}
  subC a b = case a - b of {c@Inf_ → (# Inf_, T #); c → (# c , F #)}
instance ℤ F64 where
  negate = negateDouble#; (-) = (-##)
  (//) = (/##); _ %% _ = 0.0##
  x //%% y = (# x / y , 0.0## #)
instance ℝ F64 where
  abs = fabsDouble#
  exp = expDouble#; log = logDouble#
  sqrt = sqrtDouble#
  sin = sinDouble#; cos = cosDouble#; tan = tanDouble#
  asin = asinDouble#; acos = acosDouble#; atan = atanDouble#
  sinh = sinhDouble#; cosh = coshDouble#; tanh = tanhDouble#
  (**) = (**##)

decode2I ∷ F64 → (# I8, U32, U32, I16 #) -- ^ (sign {1,-1}, high, low, exp)
decode2I = coerce decodeDouble_2Int#  
decodeI64 ∷ F64 → (# I64, I16 #) -- ^ (mantissa , base-2 exponent)
decodeI64 = coerce decodeDouble_Int64#

fromI ∷ I → F64
fromI = int2Double#
toI ∷ F64 → I
toI = double2Int#
fromU ∷ U → F64
fromU = word2Double#
toF32 ∷ F64 → F32
toF32 = double2Float#
fromF32 ∷ F32 → F64
fromF32 = float2Double#

pattern Inf ∷ F64
pattern Inf ← (((1.0## / 0.0##) ≡) → T) where Inf = 1.0## / 0.0##
pattern Inf_ ∷ F64
pattern Inf_ ← (((-1.0## / 0.0##) ≡) → T) where Inf_ = -1.0## / 0.0##

pattern Max ∷ F64
pattern Max ← f64_max where Max = case f64_max of D# x → x
pattern Min ∷ F64
pattern Min ← f64_min where Min = case f64_min of D# x → x
pattern Eps ← ((\x → f64_epsilon == D# x) → True) where Eps = case f64_epsilon of D# x → x
