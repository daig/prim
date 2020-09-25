module I32 (I32, module I32) where

fromI64 ∷ I64 → I32
fromI64 = narrow32Int#

add, sub, mul, quot, rem ∷ I32 → I32 → I32
add y x = narrow32Int# (x +# y)
sub y x = narrow32Int# (x -# y)
mul y x = narrow32Int# (x *# y)
quot y x = narrow32Int# (quotInt# x y)
rem y x = narrow32Int# (remInt# x y)

quotRem ∷ I32 → I32 → (# I32, I32 #)
quotRem y x = case quotRemInt# x y of
  (# q, r #) → (# narrow32Int# q, narrow32Int# r #)

shiftL# ∷ I64 → I32 → I32
shiftL# i x = narrow32Int# (uncheckedIShiftL# x i)
