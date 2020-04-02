module RTS.Prefetch.Ref where
import qualified Ref

t0, t1, t2, t3 :: Ref.Byte -> I64 {- ^ offset -} -> ST_ s
t0 = prefetchAddr0#
t1 = prefetchAddr1#
t2 = prefetchAddr2#
t3 = prefetchAddr3#
