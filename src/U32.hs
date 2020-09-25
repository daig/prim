module U32 (U32, module U32) where

fromU64 ∷ U64 → U32
fromU64 = narrow32Word#

add, sub, mul ∷ U32 → U32 → U32
add y x = narrow32Word# (plusWord# x y)
sub y x = narrow32Word# (minusWord# x y)
mul y x = narrow32Word# (timesWord# x y)

pattern Max, Min ∷ U32
pattern Max =  0xFFFFFFFF##
pattern Min = 0##
