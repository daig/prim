module I.I32 (I32#, module I.I32) where

type I32 = I

fromI ∷ I → I32
fromI = narrow32Int#


(+), (-), (*) ∷ I32 → I32 → I32
x + y = narrow32Int# (x +# y)
x - y = narrow32Int# (x -# y)
x * y = narrow32Int# (x *# y)
add, sub, mul, quot, rem ∷ I32 → I32 → I32
add y x = narrow32Int# (x +# y)
sub y x = narrow32Int# (x -# y)
mul y x = narrow32Int# (x *# y)
quot y x = narrow32Int# (quotInt# x y)
rem y x = narrow32Int# (remInt# x y)

quotRem ∷ I32 → I32 → (# I32, I32 #)
quotRem y x = case quotRemInt# x y of
  (# q, r #) → (# narrow32Int# q, narrow32Int# r #)

shiftL# ∷ I → I32 → I32
shiftL# i x = narrow32Int# (uncheckedIShiftL# x i)

pattern Max, Min ∷ I32
pattern Max =  0x7FFFFFFF#
pattern Min = -0x80000000#