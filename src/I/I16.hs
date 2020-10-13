module I.I16 (I16#, module I.I16) where

fromI ∷ I → I16#
fromI = narrow16Int#

(+), (-), (*) ∷ I16# → I16# → I16#
x + y = narrow16Int# (x +# y)
x - y = narrow16Int# (x -# y)
x * y = narrow16Int# (x *# y)
add, sub, mul, quot, rem ∷ I16# → I16# → I16#
add y x = narrow16Int# (x +# y)
sub y x = narrow16Int# (x -# y)
mul y x = narrow16Int# (x *# y)
quot y x = narrow16Int# (quotInt# x y)
rem y x = narrow16Int# (remInt# x y)

quotRem ∷ I16# → I16# → (# I16#, I16# #)
quotRem y x = case quotRemInt# x y of
  (# q, r #) → (# narrow16Int# q, narrow16Int# r #)

shiftL# ∷ I → I16# → I16#
shiftL# i x = narrow16Int# (uncheckedIShiftL# x i)

pattern Max, Min ∷ I16#
pattern Max =  0x7FFF#
pattern Min = -0x800#
