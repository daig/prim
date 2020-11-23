module RTS.Prefetch.P where
import qualified P.Byte as Byte

t0, t1, t2, t3 ∷ Byte.P → I {- ^ offset -} → ST_# s
t0 = prefetchAddr0#
t1 = prefetchAddr1#
t2 = prefetchAddr2#
t3 = prefetchAddr3#
