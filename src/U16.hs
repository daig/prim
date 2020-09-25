module U16 (U16, module U16) where

fromU64 ∷ U64 → U16
fromU64 = narrow16Word#

add, sub, mul ∷ U16 → U16 → U16
add y x = narrow16Word# (plusWord# x y)
sub y x = narrow16Word# (minusWord# x y)
mul y x = narrow16Word# (timesWord# x y)

pattern Max, Min ∷ U16
pattern Max = 0xFFFF##
pattern Min = 0##
