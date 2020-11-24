module U32 (U32(U32#,U32),module U32) where

-- | Narrow a machine 'U' to 32 bits
pattern U32 ∷ U → U32
pattern U32 i ← (coerce narrow32Word# → i) where U32 = coerce
{-# complete U32 #-}

(+),(-),(×) ∷ U32 → U32 → U32
x + y = U32 (coerce plusWord# x y)
x - y = U32 (coerce minusWord# x y)
x × y = U32 (coerce timesWord# x y)

pattern Max, Min ∷ U32
pattern Max =  U32# 0xFFFFFFFF##
pattern Min = U32# 0##

-- × Bitwise operations

-- | Count the number of set bits
popCnt,clz,ctz ∷ U32 → U32
popCnt = coerce popCnt32#; clz = coerce clz32#; ctz = coerce ctz32#

byteSwap ∷ U32 → U32
byteSwap = coerce byteSwap32#

pext ∷ U → U32 → U32
pext = coerce pext32#
pdep ∷ U → U32 → U
pdep = coerce pdep32#

-- | Reverse the order of the bits.
reverse ∷ U32 → U32
reverse = coerce bitReverse32#
