module U8 (U8, module U8) where

fromU64 :: U64 -> U8
fromU64 = narrow8Word#

add, sub, mul :: U8 -> U8 -> U8
add y x = narrow8Word# (plusWord# x y)
sub y x = narrow8Word# (minusWord# x y)
mul y x = narrow8Word# (timesWord# x y)
