module RTS.Prefetch.Array where
import qualified Array.Byte as Byte

t0, t1, t2, t3 ∷ Byte.A → I {- ^ offset -} → ST_# s
t0 = prefetchByteArray0#
t1 = prefetchByteArray1#
t2 = prefetchByteArray2#
t3 = prefetchByteArray3#
