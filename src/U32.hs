module U32 (U32, module U32) where

fromU64 ∷ U64 → U32
fromU64 = narrow32Word#

(+),(-),(*) ∷ U32 → U32 → U32
x + y = narrow32Word# (plusWord# x y)
x - y = narrow32Word# (minusWord# x y)
x * y = narrow32Word# (timesWord# x y)
add, sub, mul ∷ U32 → U32 → U32
add y x = narrow32Word# (plusWord# x y)
sub y x = narrow32Word# (minusWord# x y)
mul y x = narrow32Word# (timesWord# x y)

pattern Max, Min ∷ U32
pattern Max =  0xFFFFFFFF##
pattern Min = 0##

-- * Bitwise operations

-- | Count the number of set bits
popCnt,clz,ctz ∷ B32 → B32
popCnt = popCnt32#; clz = clz32#; ctz = ctz32#

byteSwap ∷ B32 → B32
byteSwap = byteSwap32#

pext ∷ B32 → U64 → B32
pext y x = pext32# x y
pdep ∷ B32 → U64 → U64
pdep y x = pdep32# x y

-- | Reverse the order of the bits.
reverse ∷ B32 → B32
reverse = bitReverse32#
