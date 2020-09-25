module F64 (F64, (+##), (-##), (*##), (/##), (>##), (>=##), (<#), (<=#), (==##)
  ,module F64) where

add,sub,mul,div :: F64 -> F64 -> F64
add y x = x +## y
sub y x = x -## y
-- | Low word of signed integer multiply
mul y x = x *## y
div y x = x /## y
negate,abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh :: F64 -> F64
negate = negateDouble#
abs = fabsDouble#
exp = expDouble#; log = logDouble#
sqrt = sqrtDouble#
sin = sinDouble#; cos = cosDouble#; tan = tanDouble#
asin = asinDouble#; acos = acosDouble#; atan = atanDouble#
sinh = sinhDouble#; cosh = coshDouble#; tanh = tanhDouble#

pow :: F64 -> F64 -> F64
pow y x = x **## y

decode2Int :: F64 -> (# Int, U64, U64, Int #)
decode2Int = decodeDouble_2Int#  
decodeI64 :: F64 -> (# Int, Int #)
decodeI64 = decodeDouble_Int64#


gt y x = x >## y
ge y x = x >=## y
lt y x = x <## y
le y x = x <=## y
eq x y = x ==## y
ne x y = x /=## y

fromInt :: Int -> F64
fromInt = int2Double#
toInt :: F64 -> Int
toInt = double2Int#
fromU64 :: U64 -> F64
fromU64 = word2Double#
toF32 :: F64 -> F32
toF32 = double2Float#
fromF32 :: F32 -> F64
fromF32 = float2Double#
