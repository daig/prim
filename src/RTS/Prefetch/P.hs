module RTS.Prefetch.P where
import P


t0, t1, t2, t3 ∷ P → I {- ^ offset -} → ST_# s
t0 = prefetchAddr0#
t1 = prefetchAddr1#
t2 = prefetchAddr2#
t3 = prefetchAddr3#
