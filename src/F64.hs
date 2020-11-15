module F64 (F64, (+##), (-##), (*##), (/##), (>##), (>=##), (<#), (<=#), (==##)
  ,module F64) where


(+),(-),(*),(/) ∷ F64 → F64 → F64
(+) = (+##); (-) = (-##); (*) = (*##); (/) = (/##)
infixl 7 *, /
infixl 6 +, -
add,sub,mul,div ∷ F64 → F64 → F64
add y x = x +## y
sub y x = x -## y
-- | Low word of signed integer multiply
mul y x = x *## y
div y x = x /## y
negate,abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ F64 → F64
negate = negateDouble#
abs = fabsDouble#
exp = expDouble#; log = logDouble#
sqrt = sqrtDouble#
sin = sinDouble#; cos = cosDouble#; tan = tanDouble#
asin = asinDouble#; acos = acosDouble#; atan = atanDouble#
sinh = sinhDouble#; cosh = coshDouble#; tanh = tanhDouble#

pow ∷ F64 → F64 → F64
pow y x = x **## y

decode2I ∷ F64 → (# I8#, U32#, U32#, I16# #) -- ^ (sign {1,-1}, high, low, exp)
decode2I = decodeDouble_2Int#  
decodeI64 ∷ F64 → (# I64, I16# #) -- ^ (mantissa , base-2 exponent)
decodeI64 = decodeDouble_Int64#


infix 4 >, ≥, <, ≤, ≡, ≠
(>),(≥),(<),(≤),(≡),(≠)
  ,gt, ge, lt, le, eq, ne ∷ F64 → F64 → B#
(>) = (>##); (≥) = (>=##); (<) = (<##); (≤) = (<=##)
(≡) = (==##); (≠) = (/=##)
gt = (<##); ge = (<=##); lt = (>##); le = (>=##); eq = (==##); ne = (/=##)

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
