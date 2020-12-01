module F64 (F64, (+##), (-##), (*##), (/##), (>##), (>=##), (<#), (<=#), (==##)
  ,module F64) where
import I16 (I16(..))
import I8 (I8(..))
import I64 (I64(..))

instance (≡) F64 where (≡) = coerce (==##); (≠) = coerce (/=##)
instance (≤) F64 where (>) = coerce (>##); (≥) = coerce (>=##); (<) = coerce (<##); (≤) = coerce (<=##)
instance ℕ F64 where
  (+) = (+##); (×) = (*##)
  (/) = (/##); _ % _ = 0.0##
  x /% y = (# x / y , 0.0## #)
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
