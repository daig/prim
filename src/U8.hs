module U8 (U8, module U8) where

fromU64 ∷ U64 → U8
fromU64 = narrow8Word#

(+),(-),(*) ∷ U8 → U8 → U8
x + y = narrow8Word# (plusWord# x y)
x - y = narrow8Word# (minusWord# x y)
x * y = narrow8Word# (timesWord# x y)
add, sub, mul ∷ U8 → U8 → U8
add y x = narrow8Word# (plusWord# x y)
sub y x = narrow8Word# (minusWord# x y)
mul y x = narrow8Word# (timesWord# x y)

pattern Max, Min ∷ U8
pattern Max = 0xFF##
pattern Min = 0##

-- * Bitwise operations
-- | Count the number of set bits
popCnt,clz,ctz ∷ B8 → B8
popCnt = popCnt8#; clz = clz8#; ctz = ctz8#

pext ∷ B8 → U64 → B8
pext y x = pext8# x y
pdep ∷ B8 → U64 → U64
pdep y x = pdep8# x y

-- | Reverse the order of the bits.
reverse ∷ B8 → B8
reverse = bitReverse8#
