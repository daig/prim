{-# language CPP #-}
module Array.Atomic where
import Prim
import Prim.Atomic
import Cast
import Cmp
import Coerce


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

instance Cas x s (P_Box s x) where
  cas r old new s0 = case casMutVar# r old new s0 of
    (# s1, failed', a #) → (# s1, case failed' of {0# → old; 1# → a} #)

instance Cas x s (# A_Box_M x s, I #) where
  cas (# coerce → m, i #) old new s0 = case casArray# m i old new s0 of
    (# s1, failed', a #) → (# s1, case failed' of {0# → old; 1# → a} #)

instance Cas x s (# A_Box_Small_M x s, I #) where
  cas (# coerce → m, i #) old new s0 = case casSmallArray# m i old new s0 of
    (# s1, failed', a #) → (# s1, case failed' of {0# → old; 1# → a} #)


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

type Num_Atomics ∷ ∀ {rx ∷ Rep} {ra ∷ Rep}. T rx → ★ → T ra → Constraint
type Logic_Atomics ∷ ∀ {rx ∷ Rep} {ra ∷ Rep}. T rx → ★ → T ra → Constraint
type Atomics ∷ ∀ {rx ∷ Rep} {ra ∷ Rep}. T rx → ★ → T ra → Constraint
class Atomics x s a | a → x where
  read_atomic ∷ a → ST s x
  write_atomic ∷ a → x → ST_ s
class Logic_Atomics x s a | a → x where
  xor_atomic, or_atomic, and_atomic, nand_atomic ∷ a → x → ST s x
class Num_Atomics x s a | a → x where
  sub_atomic, add_atomic ∷ a → x → ST s x

instance p ≑ P# ⇒ Atomics p s (P_Unbox p) where
  read_atomic = coerce (read_atomicP @P#)
  write_atomic = coerce (write_atomicP @P#)
instance p ≑ P# ⇒ Atomics p s (# A_Unbox_M p s, I #) where
  read_atomic (# m, i #) = coerce (read_atomicB @P#) m i
  write_atomic (# m, i #) = coerce (write_atomicB @P#) m i

instance Atomics U s (P_Unbox U) where
  read_atomic = coerce atomicReadWordAddr#
  write_atomic = coerce atomicWriteWordAddr#

instance Logic_Atomics U s (P_Unbox U) where
  xor_atomic = coerce fetchXorWordAddr#
  or_atomic = coerce fetchOrWordAddr#
  and_atomic = coerce fetchAndWordAddr#
  nand_atomic = coerce fetchNandWordAddr#
instance Num_Atomics U s (P_Unbox U) where
  sub_atomic = coerce fetchSubWordAddr#
  add_atomic = coerce fetchAddWordAddr#

instance Logic_Atomics U s (# A_Unbox_M U s, I #) where
  xor_atomic (# coerce → m, i #) (cast → x) s = case fetchXorIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  or_atomic (# coerce → m, i #) (cast → x) s = case fetchOrIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  and_atomic (# coerce → m, i #) (cast → x) s = case fetchAndIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  nand_atomic (# coerce → m, i #) (cast → x) s = case fetchNandIntArray# m i x s of (# s', cast → w #) → (# s', w #)
instance Num_Atomics U s (# A_Unbox_M U s, I #) where
  sub_atomic (# coerce → m, i #) (cast → x) s = case fetchSubIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  add_atomic (# coerce → m, i #) (cast → x) s = case fetchAddIntArray# m i x s of (# s', cast → w #) → (# s', w #)
instance Atomics U s (# A_Unbox_M U s, I #) where
  read_atomic (# coerce → m, i #) s = case atomicReadIntArray# m i s of (# s', cast → w #) → (# s', w #)
  write_atomic (# coerce → m, i #) (cast → x) = atomicWriteIntArray# m i x

instance Logic_Atomics I s (# A_Unbox_M I s, I #) where
  xor_atomic (# m, i #) = coerce fetchXorIntArray# m i
  or_atomic (# m, i #) = coerce fetchOrIntArray# m i
  and_atomic (# m, i #) = coerce fetchAndIntArray# m i
  nand_atomic (# m, i #) = coerce fetchNandIntArray# m i
instance Num_Atomics I s (# A_Unbox_M I s, I #) where
  sub_atomic (# m, i #) = coerce fetchSubIntArray# m i
  add_atomic (# m, i #) = coerce fetchAddIntArray# m i
instance Atomics I s (# A_Unbox_M I s, I #) where
  read_atomic (# m, i #) = coerce atomicReadIntArray# m i
  write_atomic (# m, i #) = coerce atomicWriteIntArray# m i

instance Logic_Atomics I s (P_Unbox I) where
  xor_atomic (P# p) (cast → x) s = case fetchXorWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  or_atomic (P# p) (cast → x) s = case fetchOrWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  and_atomic (P# p) (cast → x) s = case fetchAndWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  nand_atomic (P# p) (cast → x) s = case fetchNandWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
instance Num_Atomics I s (P_Unbox I) where
  sub_atomic (P# p) (cast → x) s = case fetchSubWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  add_atomic (P# p) (cast → x) s = case fetchAddWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
instance Atomics I s (P_Unbox I) where
  read_atomic (P# p) s = case atomicReadWordAddr# p s of (# s', cast → x #) → (# s', x #)
  write_atomic (P# p) (cast → x) = atomicWriteWordAddr# p x
