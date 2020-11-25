module F32 (F32, module F32) where
import I32 (I32(..))

(+),(-),(×),(/) ∷ F32 → F32 → F32
(+) = plusFloat#; (-) = minusFloat#; (×) = timesFloat#; (/) = divideFloat#
infixl 7 ×, /
infixl 6 +, -
add,sub,mul,div ∷ F32 → F32 → F32
add y x = plusFloat# x y
sub y x = minusFloat# x y
mul y x = timesFloat# x y
div y x = divideFloat# x y

negate,abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ F32 → F32
negate = negateFloat#
abs = fabsFloat#
exp = expFloat#; log = logFloat#
sqrt = sqrtFloat#
sin = sinFloat#; cos = cosFloat#; tan = tanFloat#
asin = asinFloat#; acos = acosFloat#; atan = atanFloat#
sinh = sinhFloat#; cosh = coshFloat#; tanh = tanhFloat#

pow ∷ F32 → F32 → F32
pow y x = powerFloat# x y

instance (≤) F32 where (>) = coerce gtFloat#; (≥) = coerce geFloat#; (<) = coerce ltFloat#; (≤) = coerce leFloat#
instance (≡) F32 where (≡) = coerce eqFloat#; (≠) = coerce neFloat#

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
