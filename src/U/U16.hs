module U.U16 (U16#, module U.U16) where

fromU ∷ U → U16#
fromU = narrow16Word#

(+),(-),(×) ∷ U16# → U16# → U16#
x + y = narrow16Word# (plusWord# x y)
x - y = narrow16Word# (minusWord# x y)
x × y = narrow16Word# (timesWord# x y)
add, sub, mul ∷ U16# → U16# → U16#
add y x = narrow16Word# (plusWord# x y)
sub y x = narrow16Word# (minusWord# x y)
mul y x = narrow16Word# (timesWord# x y)

pattern Max, Min ∷ U16#
pattern Max = 0xFFFF##
pattern Min = 0##

-- × Bitwise operations

-- | Count the number of set bits
popCnt,clz,ctz ∷ U16# → U16#
popCnt = popCnt16#; clz = clz16#; ctz = ctz16#

byteSwap ∷ U16# → U16#
byteSwap = byteSwap16#
pext ∷ U16# → U64 → U16#
pext y x = pext16# x y
pdep ∷ U16# → U64 → U64
pdep y x = pdep16# x y

-- | Reverse the order of the bits.
reverse ∷ U16# → U16#
reverse = bitReverse16#
