module Prim.Atomic where
import Cast
import Coerce

class Logic_Atomic (x ∷ T r) where
  xor_atomicP, or_atomicP, and_atomicP, nand_atomicP ∷ P# → x → ST s x
  xor_atomicB, or_atomicB, and_atomicB, nand_atomicB ∷ Bytes_M s → I → x → ST s x

class Num_Atomic (x ∷ T r) where
  sub_atomicB, add_atomicB ∷ Bytes_M s → I → x → ST s x
  sub_atomicP, add_atomicP ∷ P# → x → ST s x
type Atomic ∷ ∀ {r ∷ Rep}. T r → Constraint
class Atomic (x ∷ T r) where
  read_atomicP ∷ P# → ST s x
  write_atomicP ∷ P# → x → ST_ s
  swap_atomic ∷ P# → x → ST s x
  read_atomicB ∷ Bytes_M s → I →  ST s x
  write_atomicB ∷ Bytes_M s → I → x → ST_ s
instance Logic_Atomic U where
  xor_atomicP = coerce fetchXorWordAddr#
  or_atomicP = coerce fetchOrWordAddr#
  and_atomicP = coerce fetchAndWordAddr#
  nand_atomicP = coerce fetchNandWordAddr#
  xor_atomicB (coerce → m) i (cast → x) s = case fetchXorIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  or_atomicB (coerce → m) i (cast → x) s = case fetchOrIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  and_atomicB (coerce → m) i (cast → x) s = case fetchAndIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  nand_atomicB (coerce → m) i (cast → x) s = case fetchNandIntArray# m i x s of (# s', cast → w #) → (# s', w #)
instance Num_Atomic U where
  sub_atomicP = coerce fetchSubWordAddr#
  add_atomicP = coerce fetchAddWordAddr#
  sub_atomicB (coerce → m) i (cast → x) s = case fetchSubIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  add_atomicB (coerce → m) i (cast → x) s = case fetchAddIntArray# m i x s of (# s', cast → w #) → (# s', w #)
instance Atomic U where
  read_atomicP = coerce atomicReadWordAddr#
  write_atomicP = coerce atomicWriteWordAddr#
  swap_atomic = atomicExchangeWordAddr#
  read_atomicB (coerce → m) i s = case atomicReadIntArray# m i s of (# s', cast → w #) → (# s', w #)
  write_atomicB (coerce → m) i (cast → x) = atomicWriteIntArray# m i x

instance Atomic P# where
  swap_atomic = atomicExchangeAddrAddr#
  read_atomicB (coerce → m) i s = case atomicReadIntArray# m i s of (# s', (cast → p) #) → (# s', p #)
  read_atomicP (coerce → p) s = case atomicReadWordAddr# p s of (# s', (cast @I → (cast @P# → p)) #) → (# s', p #)
  write_atomicB (coerce → m) i (cast → x) = atomicWriteIntArray# m i x
  write_atomicP (coerce → p) (cast @I → (cast @U → x)) = atomicWriteWordAddr# p x

instance Logic_Atomic I where
  xor_atomicB m i = coerce fetchXorIntArray# m i
  or_atomicB m i = coerce fetchOrIntArray# m i
  and_atomicB m i = coerce fetchAndIntArray# m i
  nand_atomicB m i = coerce fetchNandIntArray# m i
  xor_atomicP p (cast → x) s = case fetchXorWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  or_atomicP p (cast → x) s = case fetchOrWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  and_atomicP p (cast → x) s = case fetchAndWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  nand_atomicP p (cast → x) s = case fetchNandWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
instance Num_Atomic I where
  sub_atomicB m i = coerce fetchSubIntArray# m i
  add_atomicB m i = coerce fetchAddIntArray# m i
  sub_atomicP p (cast → x) s = case fetchSubWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  add_atomicP p (cast → x) s = case fetchAddWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
instance Atomic I where
  read_atomicB m i = coerce atomicReadIntArray# m i
  write_atomicB m i = coerce atomicWriteIntArray# m i
  read_atomicP p s = case atomicReadWordAddr# p s of (# s', cast → x #) → (# s', x #)
  write_atomicP (coerce → p) (cast → x) = atomicWriteWordAddr# p x
  swap_atomic (coerce → p) i s = case atomicExchangeWordAddr# p (cast i) s of (# s', (cast → i') #) → (# s', i' #)


-- | Bit shuffling operations
type Eq_Atomic ∷ ∀ {r ∷ Rep}. T r → Constraint
class Eq_Atomic a where
  -- | Atomic compare-and-swap i.e. write the new value if the current value matches the provided old value.
  -- Implies a full memory barrier.
  casP ∷ P# {-^ size-aligned pointer -}
       → a {- ^ expected old value -}
       → a {- ^ new value -}
       → ST s a {- ^ the original value inside -}
  -- | Atomic compare-and-swap i.e. write the new value if the current value matches the provided old value.
  -- Implies a full memory barrier.
  casA ∷ Bytes_M s
       → I {- ^ offset in elements -}
       → a {- ^ expected old value -}
       → a {- ^ new value -}
       → ST s a {- ^ the original value inside -}

instance Eq_Atomic U where
  casP = atomicCasWordAddr#
  casA m i x0 x1 s = case casA m i (cast @I x0) (cast @I x1) s of (# s', x #) -> (# s', cast @U x #)
instance Eq_Atomic U8 where
  casP = atomicCasWord8Addr#
  casA m i x0 x1 s = case casA m i (cast @I8 x0) (cast @I8 x1) s of (# s', x #) -> (# s', cast @U8 x #)
instance Eq_Atomic U16 where
  casP = atomicCasWord16Addr#
  casA m i x0 x1 s = case casA m i (cast @I16 x0) (cast @I16 x1) s of (# s', x #) -> (# s', cast @U16 x #)
instance Eq_Atomic U32 where
  casP = atomicCasWord32Addr#
  casA m i x0 x1 s = case casA m i (cast @I32 x0) (cast @I32 x1) s of (# s', x #) -> (# s', cast @U32 x #)
instance Eq_Atomic U64 where
  casP = atomicCasWord64Addr#
  casA m i x0 x1 s = case casA m i (cast @I64 x0) (cast @I64 x1) s of (# s', x #) -> (# s', cast @U64 x #)
instance Eq_Atomic I where
  casA = coerce casIntArray#
  casP p x0 x1 s = case casP p (cast @U x0) (cast @U x1) s of (# s', x #) -> (# s', cast @I x #)
instance Eq_Atomic I8 where
  casP p x0 x1 s = case casP p (cast @U8 x0) (cast @U8 x1) s of (# s', x #) -> (# s', cast @I8 x #)
  casA = coerce casInt8Array#
instance Eq_Atomic I16 where
  casP p x0 x1 s = case casP p (cast @U16 x0) (cast @U16 x1) s of (# s', x #) -> (# s', cast @I16 x #)
  casA = coerce casInt16Array#
instance Eq_Atomic I32 where
  casP p x0 x1 s = case casP p (cast @U32 x0) (cast @U32 x1) s of (# s', x #) -> (# s', cast @I32 x #)
  casA = coerce casInt32Array#
instance Eq_Atomic I64 where
  casP p x0 x1 s = case casP p (cast @U64 x0) (cast @U64 x1) s of (# s', x #) -> (# s', cast @I64 x #)
  casA = coerce casInt64Array#
instance Eq_Atomic P# where
  casP = coerce atomicCasAddrAddr#
  casA (coerce → m) i (cast → x0) (cast → x1) s = case casIntArray# m i x0 x1 s of (# s', (cast → x) #) → (# s', x #)
