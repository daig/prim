module RTS.Prefetch where
import qualified A.Byte as Byte

class Prefetch (a ∷ T_ r) s where
  t0,t1,t2,t3 ∷ a → ST_# s

-- | W/ Byte Offset
instance Prefetch (# P, I #) s where
  t0 (# a , i #) = prefetchAddr0# a i
  t1 (# a , i #) = prefetchAddr1# a i
  t2 (# a , i #) = prefetchAddr2# a i
  t3 (# a , i #) = prefetchAddr3# a i
instance Prefetch P s where
  t0 = (`prefetchAddr0#` 0#)
  t1 = (`prefetchAddr1#` 0#)
  t2 = (`prefetchAddr2#` 0#)
  t3 = (`prefetchAddr3#` 0#)
-- | W/ Byte Offset
instance Prefetch (# Byte.A, I #) s where
  t0 (# a , i #) = prefetchByteArray0# a i
  t1 (# a , i #) = prefetchByteArray1# a i
  t2 (# a , i #) = prefetchByteArray2# a i
  t3 (# a , i #) = prefetchByteArray3# a i
instance Prefetch Byte.A s where
  t0 = (`prefetchByteArray0#` 0#)
  t1 = (`prefetchByteArray1#` 0#)
  t2 = (`prefetchByteArray2#` 0#)
  t3 = (`prefetchByteArray3#` 0#)
-- | W/ byte offset
instance Prefetch (# Byte.MA s ∷ T_A, I #) s where
  t0 (# a , i #) = prefetchMutableByteArray0# a i
  t1 (# a , i #) = prefetchMutableByteArray1# a i
  t2 (# a , i #) = prefetchMutableByteArray2# a i
  t3 (# a , i #) = prefetchMutableByteArray3# a i
instance Prefetch (Byte.MA s) s where
  t0 = (`prefetchMutableByteArray0#` 0#)
  t1 = (`prefetchMutableByteArray1#` 0#)
  t2 = (`prefetchMutableByteArray2#` 0#)
  t3 = (`prefetchMutableByteArray3#` 0#)
instance Prefetch (a ∷ T) s where
  t0 = prefetchValue0#
  t1 = prefetchValue1#
  t2 = prefetchValue2#
  t3 = prefetchValue3#
