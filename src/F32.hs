module F32 (F32, module F32) where

add,sub,mul,div :: F32 -> F32 -> F32
add y x = plusFloat# x y
sub y x = minusFloat# x y
mul y x = timesFloat# x y
div y x = divideFloat# x y

negate,abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh :: F32 -> F32
negate = negateFloat#
abs = fabsFloat#
exp = expFloat#; log = logFloat#
sqrt = sqrtFloat#
sin = sinFloat#; cos = cosFloat#; tan = tanFloat#
asin = asinFloat#; acos = acosFloat#; atan = atanFloat#
sinh = sinhFloat#; cosh = coshFloat#; tanh = tanhFloat#

pow :: F32 -> F32 -> F32
pow y x = powerFloat# x y

gt,ge,lt,le,eq,ne :: F32 -> F32 -> B
gt y x = gtFloat# x y
ge y x = geFloat# x y
lt y x = ltFloat# x y
le y x = leFloat# x y
eq x y = eqFloat# x y
ne x y = neFloat# x y

toInt :: F32 -> I64
toInt = float2Int#
fromInt:: I64 -> F32
fromInt = int2Float#
toF64 :: F32 -> F64
toF64 = float2Double#
fromF64 :: F64 -> F32
fromF64 = double2Float#

decodeI64 :: F32 -> (# I64, I64 #)
decodeI64 = decodeFloat_Int#
