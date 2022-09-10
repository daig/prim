{-# language CPP #-}
module Array.Cas where
import Prim
import Cast
import Bits
import Cmp
import Coerce

class AtomicOpsType (x ∷ T r) where
  xor_atomicP, or_atomicP, and_atomicP, nand_atomicP, sub_atomicP, add_atomicP ∷ P# → x → ST s x
  read_atomicP ∷ P# → ST s x
  write_atomicP ∷ P# → x → ST_ s
  xor_atomicB, or_atomicB, and_atomicB, nand_atomicB, sub_atomicB, add_atomicB ∷ Bytes_M s → I → x → ST s x
  read_atomicB ∷ Bytes_M s → I →  ST s x
  write_atomicB ∷ Bytes_M s → I → x → ST_ s
instance AtomicOpsType U where
  xor_atomicP = coerce fetchXorWordAddr#
  or_atomicP = coerce fetchOrWordAddr#
  and_atomicP = coerce fetchAndWordAddr#
  nand_atomicP = coerce fetchNandWordAddr#
  sub_atomicP = coerce fetchSubWordAddr#
  add_atomicP = coerce fetchAddWordAddr#
  read_atomicP = coerce atomicReadWordAddr#
  write_atomicP = coerce atomicWriteWordAddr#
  xor_atomicB (coerce → m) i (cast → x) s = case fetchXorIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  or_atomicB (coerce → m) i (cast → x) s = case fetchOrIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  and_atomicB (coerce → m) i (cast → x) s = case fetchAndIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  nand_atomicB (coerce → m) i (cast → x) s = case fetchNandIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  sub_atomicB (coerce → m) i (cast → x) s = case fetchSubIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  add_atomicB (coerce → m) i (cast → x) s = case fetchAddIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  read_atomicB (coerce → m) i s = case atomicReadIntArray# m i s of (# s', cast → w #) → (# s', w #)
  write_atomicB (coerce → m) i (cast → x) = atomicWriteIntArray# m i x



instance AtomicOpsType I where
  xor_atomicB m i = coerce fetchXorIntArray# m i
  or_atomicB m i = coerce fetchOrIntArray# m i
  and_atomicB m i = coerce fetchAndIntArray# m i
  nand_atomicB m i = coerce fetchNandIntArray# m i
  sub_atomicB m i = coerce fetchSubIntArray# m i
  add_atomicB m i = coerce fetchAddIntArray# m i
  read_atomicB m i = coerce atomicReadIntArray# m i
  write_atomicB m i = coerce atomicWriteIntArray# m i
  xor_atomicP p (cast → x) s = case fetchXorWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  or_atomicP p (cast → x) s = case fetchOrWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  and_atomicP p (cast → x) s = case fetchAndWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  nand_atomicP p (cast → x) s = case fetchNandWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  sub_atomicP p (cast → x) s = case fetchSubWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  add_atomicP p (cast → x) s = case fetchAddWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  read_atomicP p s = case atomicReadWordAddr# p s of (# s', cast → x #) → (# s', x #)
  write_atomicP p (cast → x) = atomicWriteWordAddr# p x


-- | Atomic compare and swap, reporting failure.
type Cas ∷ ∀ {rx ∷ Rep} {ra ∷ Rep}. T rx → ★ → T ra → Constraint
class Cas x s a | a → x where
  -- | Compare the expected old value to the actual contained value and swap with the new value if they're equal.
  cas ∷ a → x {- ^ expected old value -}
          → x {- ^ new value -}
          → ST s x {- ^ old value -}

instance Cas U s (# A_Unbox_M U s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U8 s (# A_Unbox_M U8 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U16 s (# A_Unbox_M U16 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U32 s (# A_Unbox_M U32 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas U64 s (# A_Unbox_M U64 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I s (# A_Unbox_M I s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I8 s (# A_Unbox_M I8 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I16 s (# A_Unbox_M I16 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I32 s (# A_Unbox_M I32 s, I #) where cas (# Bytes_M m, i #) = casA m i
instance Cas I64 s (# A_Unbox_M I64 s, I #) where cas (# Bytes_M m, i #) = casA m i

instance Cas U s (P_Unbox U) where cas = coerce (casP @U)
instance Cas U8 s (P_Unbox U8) where cas = coerce (casP @U8)
instance Cas U16 s (P_Unbox U16) where cas = coerce (casP @U16)
instance Cas U32 s (P_Unbox U32) where cas = coerce (casP @U32)
instance Cas U64 s (P_Unbox U64) where cas = coerce (casP @U64)
instance Cas I s (P_Unbox I) where cas = coerce (casP @I)
instance Cas I8 s (P_Unbox I8) where cas = coerce (casP @I8)
instance Cas I16 s (P_Unbox I16) where cas = coerce (casP @I16)
instance Cas I32 s (P_Unbox I32) where cas = coerce (casP @I32)
instance Cas I64 s (P_Unbox I64) where cas = coerce (casP @I64)



-- | Atomic compare and swap
type Cas' ∷ ∀ {rx ∷ Rep} {ra ∷ Rep}. T rx → ★ → T ra → Constraint
class Cas' x s a | a → x where
  -- | Compare the expected old value to the actual contained value and swap with the new value if they're equal.
  cas' ∷ a → x {- ^ expected old value -}
           → x {- ^ new value -}
           → ST s (Result# x x) {- ^ @Ok newVal@ or @Err actualVal@ -}
-- | Compares via pointer equality.
instance Cas' x s (P_Box s x) where
  cas' r old new s0 = case casMutVar# r old new s0 of
    (# s1, failed', a #) → (# s1, (# B# failed', a #) #)

-- | Offset in elements
instance Cas' x s (# A_Box_M x s, I #) where
  cas' (# MutableArray# m, i #) old new s0 = case casArray# m i old new s0 of
    (# s1, failed', x #) → (# s1, (# B# failed', x #) #)

-- | Offset in elements
instance Cas' x s (# A_Box_Small_M x s, I #) where
  cas' (# SmallArray_M# m, i #) old new s0 = case casSmallArray# m i old new s0 of
    (# s1, failed', x #) → (# s1, (# B# failed', x #) #)


#define INST_A_CAS_(A)\
instance ((≡) x, x ≑ A) ⇒ Cas' x s (# A_Unbox_M x s, I #) where {\
  cas' (# Bytes_M m, i #) x0 x1 s = case coerce (casA @A) m i x0 x1 s of;\
    (# s', x #) → let failed' = x ≠ x0;\
                  in (# s', (# failed', case failed' of {F → x; T → x0} #) #)}

INST_A_CAS_(I)
INST_A_CAS_(I8)
INST_A_CAS_(I16)
INST_A_CAS_(I32)
INST_A_CAS_(I64)

INST_A_CAS_(U)
INST_A_CAS_(U8)
INST_A_CAS_(U16)
INST_A_CAS_(U32)
INST_A_CAS_(U64)

instance ((≡) x, x ≑ U) ⇒ Cas' x s (P_Unbox x) where
  cas' (P# p) x0 x1 s = case coerce atomicCasWordAddr# p x0 x1 s of
    (# s', x #) → let failed' = x ≠ x0
                  in (# s', (# failed', case failed' of {F → x; T → x0} #) #)

instance x ≑ P# ⇒ Cas' P# s (P_Unbox x) where
  cas' (P# p) x0 x1 s = case atomicCasAddrAddr# p x0 x1 s of
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
