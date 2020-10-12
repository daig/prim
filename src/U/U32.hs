module U.U32 (U32#, module U.U32) where

fromU ∷ U → U32#
fromU = narrow32Word#

(+),(-),(*) ∷ U32# → U32# → U32#
x + y = narrow32Word# (plusWord# x y)
x - y = narrow32Word# (minusWord# x y)
x * y = narrow32Word# (timesWord# x y)
add, sub, mul ∷ U32# → U32# → U32#
add y x = narrow32Word# (plusWord# x y)
sub y x = narrow32Word# (minusWord# x y)
mul y x = narrow32Word# (timesWord# x y)

pattern Max, Min ∷ U32#
pattern Max =  0xFFFFFFFF##
pattern Min = 0##

-- * Bitwise operations

-- | Count the number of set bits
popCnt,clz,ctz ∷ U32# → U32#
popCnt = popCnt32#; clz = clz32#; ctz = ctz32#

byteSwap ∷ U32# → U32#
byteSwap = byteSwap32#

pext ∷ U32# → U → U32#
pext y x = pext32# x y
pdep ∷ U32# → U → U
pdep y x = pdep32# x y

-- | Reverse the order of the bits.
reverse ∷ U32# → U32#
reverse = bitReverse32#
