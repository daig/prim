{-# language CPP #-}
module Array.Cas where
import Prim
import Cast
import Bits

-- | Atomic compare and swap
type Cas ∷ ∀ {rx ∷ Rep} {ra ∷ Rep} {rr ∷ Rep}. T rx → T ra → ★ → T rr → Constraint
class Cas x y s a | a → x y where
  -- | Compare the expected old value to the actual contained value and swap with the new value if they're equal.
  cas ∷ a → x {- ^ expected old value -}
          → x {- ^ new value -}
          → ST s y {- ^ the original value inside -}
-- | Returns @Ok newVal@ on success or @Err actualOldVal@ if @actualOldVal ≠ expectedOldVal@.
-- Compares via pointer equality.
instance Cas x (Result# x x) s (P_Box s x) where
  cas r old new s0 = case casMutVar# r old new s0 of
    (# s1, failed', a #) → (# s1, (# B# failed', a #) #)

-- | Offset in elements
instance Cas x (Result# x x) s (# A_Box_M x s, I #) where
  cas (# MutableArray# m, i #) old new s0 = case casArray# m i old new s0 of
    (# s1, failed', x #) → (# s1, (# B# failed', x #) #)

-- | Offset in elements
instance Cas x (Result# x x) s (# A_Box_Small_M x s, I #) where
  cas (# SmallArray_M# m, i #) old new s0 = case casSmallArray# m i old new s0 of
    (# s1, failed', x #) → (# s1, (# B# failed', x #) #)

instance Cas I I s (# A_Unbox_M I s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I8 I8 s (# A_Unbox_M I8 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I16 I16 s (# A_Unbox_M I16 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I32 I32 s (# A_Unbox_M I32 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I64 I64 s (# A_Unbox_M I64 s, I #) where cas (# Bytes_M m, i #) = casA m i

instance Cas U U s (# A_Unbox_M U s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U8 U8 s (# A_Unbox_M U8 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U16 U16 s (# A_Unbox_M U16 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U32 U32 s (# A_Unbox_M U32 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U64 U64 s (# A_Unbox_M U64 s, I #) where cas (# Bytes_M m, i #) = casA m i

instance Cas U U s (P_Unbox U) where cas = coerce atomicCasWordAddr#
instance Cas P# P# s (P_Unbox P#) where cas = coerce atomicCasAddrAddr#
#define INST_CAS_P(A)\
instance Cas (P_Unbox A) (P_Unbox A) s (P_Unbox (P_Unbox A)) where cas = coerce atomicCasAddrAddr#

INST_CAS_P(P#)
INST_CAS_P(U)
INST_CAS_P(U8)
INST_CAS_P(U16)
INST_CAS_P(U32)
INST_CAS_P(U64)
INST_CAS_P(I)
INST_CAS_P(I8)
INST_CAS_P(I16)
INST_CAS_P(I32)
INST_CAS_P(I64)
INST_CAS_P(F32)
INST_CAS_P(F64)
INST_CAS_P(Char)
INST_CAS_P(Char8)
{-
-- | Compare and swap if the old value matches expected.
cas' ∷ P s a
    → a -- ^ expected old value
    → a -- ^ new value
    → ST s (Result#  a a) -- ^ Whether the swap failed, and the actual new value
cas' r old new s0 = case casMutVar#  r old new s0 of
  (#  s1, failed', a #) → (# s1, (# B# failed', a #) #)

  -}
