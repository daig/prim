module I.I8 (I8#, module I.I8) where

fromI ∷ I → I8#
fromI = narrow8Int#

(+), (-), (*) ∷ I8# → I8# → I8#
x + y = narrow8Int# (x +# y)
x - y = narrow8Int# (x -# y)
x * y = narrow8Int# (x *# y)
add, sub, mul, quot, rem ∷ I8# → I8# → I8#
add y x = narrow8Int# (x +# y)
sub y x = narrow8Int# (x -# y)
mul y x = narrow8Int# (x *# y)
quot y x = narrow8Int# (quotInt# x y)
rem y x = narrow8Int# (remInt# x y)

quotRem ∷ I8# → I8# → (# I8#, I8# #)
quotRem y x = case quotRemInt# x y of
  (# q, r #) → (# narrow8Int# q, narrow8Int# r #)

shiftL# ∷ I → I8# → I8#
shiftL# i x = narrow8Int# (uncheckedIShiftL# x i)

pattern Max, Min ∷ I8#
pattern Max =  0x7F#
pattern Min = -0x80#
