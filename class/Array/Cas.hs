{-# language CPP #-}
module Array.Cas where
import Prim
import Cast
import Bits
import Cmp
import Coerce

-- | Atomic compare and swap
type Cas ∷ ∀ {rx ∷ Rep} {ra ∷ Rep}. T rx → ★ → T ra → Constraint
class Cas x s a | a → x where
  -- | Compare the expected old value to the actual contained value and swap with the new value if they're equal.
  cas ∷ a → x {- ^ expected old value -}
          → x {- ^ new value -}
          → ST s (Result# x x) {- ^ @Ok newVal@ or @Err actualVal@ -}
-- | Returns @Ok newVal@ on success or @Err actualOldVal@ if @actualOldVal ≠ expectedOldVal@.
-- Compares via pointer equality.
instance Cas x s (P_Box s x) where
  cas r old new s0 = case casMutVar# r old new s0 of
    (# s1, failed', a #) → (# s1, (# B# failed', a #) #)

-- | Offset in elements
instance Cas x s (# A_Box_M x s, I #) where
  cas (# MutableArray# m, i #) old new s0 = case casArray# m i old new s0 of
    (# s1, failed', x #) → (# s1, (# B# failed', x #) #)

-- | Offset in elements
instance Cas x s (# A_Box_Small_M x s, I #) where
  cas (# SmallArray_M# m, i #) old new s0 = case casSmallArray# m i old new s0 of
    (# s1, failed', x #) → (# s1, (# B# failed', x #) #)

#define INST_A_CAS(A)\
instance ((≡) x, x ≑ A) ⇒ Cas x s (# A_Unbox_M x s, I #) where {\
  cas (# Bytes_M m, i #) x0 x1 s = case coerce (casA @_ @A) m i x0 x1 s of;\
    (# s', x #) → let failed' = x ≠ x0;\
                  in (# s', (# failed', case failed' of {F → x; T → x0} #) #)}

INST_A_CAS(I)
INST_A_CAS(I8)
INST_A_CAS(I16)
INST_A_CAS(I32)
INST_A_CAS(I64)

INST_A_CAS(U)
INST_A_CAS(U8)
INST_A_CAS(U16)
INST_A_CAS(U32)
INST_A_CAS(U64)

instance ((≡) x, x ≑ U) ⇒ Cas x s (P_Unbox x) where
  cas (P# p) x0 x1 s = case coerce atomicCasWordAddr# p x0 x1 s of
    (# s', x #) → let failed' = x ≠ x0
                  in (# s', (# failed', case failed' of {F → x; T → x0} #) #)

instance x ≑ P# ⇒ Cas P# s (P_Unbox x) where
  cas (P# p) x0 x1 s = case atomicCasAddrAddr# p x0 x1 s of
    (# s', x #) → let failed' = x ≠ x0
                  in (# s', (# failed', case failed' of {F → x; T → x0} #) #)

type AtomicOps ∷ ∀ {rx ∷ Rep} {ra ∷ Rep}. T rx → ★ → T ra → Constraint
class AtomicOps x s a | a → x where
  xor_atomic, or_atomic, and_atomic, nand_atomic, sub_atomic, add_atomic ∷ a → x → ST s x
  read_atomic ∷ a → ST s x
  write_atomic ∷ a → x → ST_ s

class AtomicSwap (x ∷ T r) where swap ∷ P_Unbox x → x → ST s x
instance x ≑ U ⇒ AtomicSwap x where swap = coerce atomicExchangeWordAddr#
instance x ≑ P# ⇒ AtomicSwap x where swap = coerce atomicExchangeAddrAddr#
deriving via P# instance AtomicSwap (P_Unbox P#)

instance AtomicOps U s (P_Unbox U) where
  xor_atomic = coerce fetchXorWordAddr#
  or_atomic = coerce fetchOrWordAddr#
  and_atomic = coerce fetchAndWordAddr#
  nand_atomic = coerce fetchNandWordAddr#
  sub_atomic = coerce fetchSubWordAddr#
  add_atomic = coerce fetchAddWordAddr#
  read_atomic = coerce atomicReadWordAddr#
  write_atomic = coerce atomicWriteWordAddr#

instance AtomicOps U s (# A_Unbox_M U s, I #) where
  xor_atomic (# coerce → m, i #) (cast → x) s = case fetchXorIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  or_atomic (# coerce → m, i #) (cast → x) s = case fetchOrIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  and_atomic (# coerce → m, i #) (cast → x) s = case fetchAndIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  nand_atomic (# coerce → m, i #) (cast → x) s = case fetchNandIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  sub_atomic (# coerce → m, i #) (cast → x) s = case fetchSubIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  add_atomic (# coerce → m, i #) (cast → x) s = case fetchAddIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  read_atomic (# coerce → m, i #) s = case atomicReadIntArray# m i s of (# s', cast → w #) → (# s', w #)
  write_atomic (# coerce → m, i #) (cast → x) = atomicWriteIntArray# m i x

instance AtomicOps I s (# A_Unbox_M I s, I #) where
  xor_atomic (# m, i #) = coerce fetchXorIntArray# m i
  or_atomic (# m, i #) = coerce fetchOrIntArray# m i
  and_atomic (# m, i #) = coerce fetchAndIntArray# m i
  nand_atomic (# m, i #) = coerce fetchNandIntArray# m i
  sub_atomic (# m, i #) = coerce fetchSubIntArray# m i
  add_atomic (# m, i #) = coerce fetchAddIntArray# m i
  read_atomic (# m, i #) = coerce atomicReadIntArray# m i
  write_atomic (# m, i #) = coerce atomicWriteIntArray# m i

instance AtomicOps I s (P_Unbox I) where
  xor_atomic (P# p) (cast → x) s = case fetchXorWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  or_atomic (P# p) (cast → x) s = case fetchOrWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  and_atomic (P# p) (cast → x) s = case fetchAndWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  nand_atomic (P# p) (cast → x) s = case fetchNandWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  sub_atomic (P# p) (cast → x) s = case fetchSubWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  add_atomic (P# p) (cast → x) s = case fetchAddWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  read_atomic (P# p) s = case atomicReadWordAddr# p s of (# s', cast → x #) → (# s', x #)
  write_atomic (P# p) (cast → x) = atomicWriteWordAddr# p x
