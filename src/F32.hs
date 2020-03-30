module F32 where

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

--decode2Int :: F32 -> (# Int, Word, Word, Int #)
--decode2Int = decodeDouble_2Int#  
--decodeI64 = F32 -> (# Int, Int #)
--decodeI64 = decodeDouble_I64#


gt y x = B# do gtFloat# x y
ge y x = B# do geFloat# x y
lt y x = B# do ltFloat# x y
le y x = B# do leFloat# x y
eq x y = B# do eqFloat# x y
ne x y = B# do neFloat# x y

toInt :: F32 -> Int
toInt = float2Int#
fromInt:: Int -> F32
fromInt = int2Float#
toF64 :: F32 -> F64
toF64 = float2Double#
fromF64 :: F64 -> F32
fromF64 = double2Float#

decodeInt :: F32 -> (# Int, Int #)
decodeInt = decodeFloat_Int#
