{-# language DerivingVia #-}
module RTS.Prefetch where
import Prim.A.Prim
import P

class Prefetch (a ∷ T_ r) s where
  t0,t1,t2,t3 ∷ a → ST_# s

-- | W/ Byte Offset
instance Prefetch (# P#, I #) s where
  t0 (# a , i #) = prefetchAddr0# a i
  t1 (# a , i #) = prefetchAddr1# a i
  t2 (# a , i #) = prefetchAddr2# a i
  t3 (# a , i #) = prefetchAddr3# a i
instance Prefetch P# s where
  t0 = (`prefetchAddr0#` 0#)
  t1 = (`prefetchAddr1#` 0#)
  t2 = (`prefetchAddr2#` 0#)
  t3 = (`prefetchAddr3#` 0#)
instance Prefetch (# (P (x ∷ T_ r)), I #) s where
  t0 (# a , i #) = coerce prefetchAddr0# a i
  t1 (# a , i #) = coerce prefetchAddr1# a i
  t2 (# a , i #) = coerce prefetchAddr2# a i
  t3 (# a , i #) = coerce prefetchAddr3# a i
instance Prefetch (P (x ∷ T_ r)) s where
  t0 = coerce (`prefetchAddr0#` 0#)
  t1 = coerce (`prefetchAddr1#` 0#)
  t2 = coerce (`prefetchAddr2#` 0#)
  t3 = coerce (`prefetchAddr3#` 0#)
-- | W/ Byte Offset
instance Prefetch (# A x, I #) s where
  t0 (# a , i #) = coerce prefetchByteArray0# a i
  t1 (# a , i #) = coerce prefetchByteArray1# a i
  t2 (# a , i #) = coerce prefetchByteArray2# a i
  t3 (# a , i #) = coerce prefetchByteArray3# a i
instance Prefetch (A x) s where
  t0 = coerce (`prefetchByteArray0#` 0#)
  t1 = coerce (`prefetchByteArray1#` 0#)
  t2 = coerce (`prefetchByteArray2#` 0#)
  t3 = coerce (`prefetchByteArray3#` 0#)
-- | W/ byte offset
instance Prefetch (# MA s x, I #) s where
  t0 (# a , i #) = coerce prefetchMutableByteArray0# a i
  t1 (# a , i #) = coerce prefetchMutableByteArray1# a i
  t2 (# a , i #) = coerce prefetchMutableByteArray2# a i
  t3 (# a , i #) = coerce prefetchMutableByteArray3# a i
instance Prefetch (MA s x) s where
  t0 = coerce (`prefetchMutableByteArray0#` 0#)
  t1 = coerce (`prefetchMutableByteArray1#` 0#)
  t2 = coerce (`prefetchMutableByteArray2#` 0#)
  t3 = coerce (`prefetchMutableByteArray3#` 0#)
instance Prefetch (a ∷ T) s where
  t0 = prefetchValue0#
  t1 = prefetchValue1#
  t2 = prefetchValue2#
  t3 = prefetchValue3#
