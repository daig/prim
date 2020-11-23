module U.U16 (U16(U16#,U16), module U.U16) where

-- | Narrow a machine 'U' to 16 bits
pattern U16 ∷ U → U16
pattern U16 i ← (coerce narrow16Word# → i) where U16 = coerce
{-# complete U16 #-}

(+),(-),(×) ∷ U16 → U16 → U16
x + y = U16 (coerce plusWord# x y)
x - y = U16 (coerce minusWord# x y)
x × y = U16 (coerce timesWord# x y)

pattern Max, Min ∷ U16
pattern Max = U16# 0xFFFF##
pattern Min = U16# 0##

-- × Bitwise operations

-- | Count the number of set bits
popCnt,clz,ctz ∷ U16 → U8
popCnt = coerce popCnt16#; clz = coerce clz16#; ctz = coerce ctz16#

byteSwap ∷ U16 → U16
byteSwap = coerce byteSwap16#
pext ∷ U64 → U16 → U16
pext = coerce pext16#
pdep ∷ U64 → U16 → U64
pdep = coerce pdep16#

-- | Reverse the order of the bits.
reverse ∷ U16 → U16
reverse = coerce bitReverse16#
