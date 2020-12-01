module F32 (F32, module F32) where
import I32 (I32(..))

instance (≡) F32 where (≡) = coerce eqFloat#; (≠) = coerce neFloat#
instance (≤) F32 where (>) = coerce gtFloat#; (≥) = coerce geFloat#; (<) = coerce ltFloat#; (≤) = coerce leFloat#
instance ℕ F32 where
  (+) = plusFloat#; (×) = timesFloat#
  (/) = divideFloat#; _ % _ = 0.0#
  x /% y = (# x / y , 0.0# #)
instance ℤ F32 where
  negate = negateFloat#; (-) = minusFloat#
  (//) = divideFloat#; _ %% _ = 0.0#
  x //%% y = (# x / y , 0.0# #)
instance ℝ F32 where
  abs = fabsFloat#
  exp = expFloat#; log = logFloat#
  sqrt = sqrtFloat#
  sin = sinFloat#; cos = cosFloat#; tan = tanFloat#
  asin = asinFloat#; acos = acosFloat#; atan = atanFloat#
  sinh = sinhFloat#; cosh = coshFloat#; tanh = tanhFloat#
  (**) = powerFloat#

toI ∷ F32 → I
toI = float2Int#
fromI ∷ I → F32
fromI = int2Float#
toF64 ∷ F32 → F64
toF64 = float2Double#
fromF64 ∷ F64 → F32
fromF64 = double2Float#

decode ∷ F32 → (# I32, I32 #)
decode = coerce decodeFloat_Int#
