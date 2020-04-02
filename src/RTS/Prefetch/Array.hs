module RTS.Prefetch.Array where
import qualified Array

t0, t1, t2, t3 :: Array.Byte -> I64 {- ^ offset -} -> ST_ s
t0 = prefetchByteArray0#
t1 = prefetchByteArray1#
t2 = prefetchByteArray2#
t3 = prefetchByteArray3#
