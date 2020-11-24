module RTS.Prefetch.Array.M where
import A

t0, t1, t2, t3 ∷ MA s → I {- ^ offset -} → ST_# s
t0 = prefetchMutableByteArray0#
t1 = prefetchMutableByteArray1#
t2 = prefetchMutableByteArray2#
t3 = prefetchMutableByteArray3#
