module Prim.Atomic where
import Cast
import Coerce

type Logic_Atomic ∷ ∀ {rx} {r}. ★ → T r → T rx → TC
class Logic_Atomic s v x | v → s x where
  (|+|=), (|=), (&=), (~&=) ∷ v → x → ST s x

class Num_Atomic (x ∷ T r) where
  (-=), (+=) ∷ P s x → x → ST s x
  sub_atomicB, add_atomicB ∷ A s x → I → x → ST s x
type Atomic ∷ ∀ {r}. T r → TC
class Atomic (x ∷ T r) where
  read_atomic ∷ P s x → ST s x
  (.=!) ∷ P s x → x → ST_ s
  swap ∷ P s x → x → ST s x
  read_atomicB ∷ A s x → I →  ST s x
  write_atomicB ∷ A s x → I → x → ST_ s

instance Logic_Atomic s (P s U) U where
  (|+|=) = coerce fetchXorWordAddr#
  (|=) = coerce fetchOrWordAddr#
  (&=) = coerce fetchAndWordAddr#
  (~&=) = coerce fetchNandWordAddr#
instance Logic_Atomic s (# A s U, I #) U where
  (# coerce → m, i #) |+|= (cast → x) = \s → case fetchXorIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  (# coerce → m, i #) |= (cast → x) = \s → case fetchOrIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  (# coerce → m, i #) &= (cast → x) = \s → case fetchAndIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  (# coerce → m, i #) ~&= (cast → x) = \s → case fetchNandIntArray# m i x s of (# s', cast → w #) → (# s', w #)
instance Num_Atomic U where
  (-=) = coerce fetchSubWordAddr#
  (+=) = coerce fetchAddWordAddr#
  sub_atomicB (coerce → m) i (cast → x) s = case fetchSubIntArray# m i x s of (# s', cast → w #) → (# s', w #)
  add_atomicB (coerce → m) i (cast → x) s = case fetchAddIntArray# m i x s of (# s', cast → w #) → (# s', w #)
instance Atomic U where
  read_atomic = coerce atomicReadWordAddr#
  (.=!) = coerce atomicWriteWordAddr#
  swap = coerce atomicExchangeWordAddr#
  (read_atomicB) (coerce → m) i s = case atomicReadIntArray# m i s of (# s', cast → w #) → (# s', w #)
  write_atomicB (coerce → m) i (cast → x) = atomicWriteIntArray# m i x

instance Atomic Addr# where
  swap = coerce atomicExchangeAddrAddr#
  (read_atomicB) (coerce → m) i s = case atomicReadIntArray# m i s of (# s', (cast @Addr# → p) #) → (# s', p #)
  read_atomic (coerce → p) s = case atomicReadWordAddr# p s of (# s', (cast @I → (cast @Addr# → p)) #) → (# s', p #)
  write_atomicB (coerce → m) i (cast @I → x) = atomicWriteIntArray# m i x
  (.=!) (coerce → p) (cast @I → (cast @U → x)) = atomicWriteWordAddr# p x

instance Logic_Atomic s (# A s I, I #) I where
  (|+|=) (# m, i #) = coerce fetchXorIntArray# m i
  (|=) (# m, i #) = coerce fetchOrIntArray# m i
  (&=) (# m, i #) = coerce fetchAndIntArray# m i
  (~&=) (# m, i #) = coerce fetchNandIntArray# m i
instance Logic_Atomic s (P s I) I where
  (|+|=) (coerce → p) (cast → x) s = case fetchXorWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  (|=) (coerce → p) (cast → x) s = case fetchOrWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  (&=) (coerce → p) (cast → x) s = case fetchAndWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  (~&=) (coerce → p) (cast → x) s = case fetchNandWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
instance Num_Atomic I where
  sub_atomicB m i = coerce fetchSubIntArray# m i
  add_atomicB m i = coerce fetchAddIntArray# m i
  (-=) (coerce → p) (cast → x) s = case fetchSubWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
  (+=) (coerce → p) (cast → x) s = case fetchAddWordAddr# p x s of (# s', (cast → w) #) → (# s', w #)
instance Atomic I where
  (read_atomicB) m i = coerce atomicReadIntArray# m i
  write_atomicB m i = coerce atomicWriteIntArray# m i
  read_atomic (coerce → p) s = case atomicReadWordAddr# p s of (# s', cast → x #) → (# s', x #)
  (.=!) (coerce → p) (cast → x) = atomicWriteWordAddr# p x
  swap (coerce → p) i s = case atomicExchangeWordAddr# p (cast i) s of (# s', (cast → i') #) → (# s', i' #)


-- | Bit shuffling operations
type Eq_Atomic ∷ ∀ {r}. T r → TC
class Eq_Atomic x where
  -- | Atomic compare-and-swap i.e. write the new value if the current value matches the provided expected old value.
  -- Implies a full memory barrier.
  casP ∷ P s x {-^ size-aligned pointer -}
       → x {- ^ expected old value -}
       → x {- ^ new value -}
       → ST s x {- ^ the original value inside -}
  -- | Atomic compare-and-swap i.e. write the new value if the current value matches the provided expected old value.
  -- Implies a full memory barrier.
  -- _Warning_: this can fail with an unchecked exception.
  casA ∷ A s x
       → I {- ^ offset in elements -}
       → x {- ^ expected old value -}
       → x {- ^ new value -}
       → ST s x {- ^ the original value inside -}

instance Eq_Atomic U where
  casP = coerce atomicCasWordAddr#
  casA (coerce → m) i x0 x1 s = case casA m i (cast @I x0) (cast @I x1) s of (# s', x #) → (# s', cast @U x #)
instance Eq_Atomic U1 where
  casP = coerce atomicCasWord8Addr#
  casA (coerce → m) i x0 x1 s = case casA m i (cast @I1 x0) (cast @I1 x1) s of (# s', x #) → (# s', cast @U1 x #)
instance Eq_Atomic U2 where
  casP = coerce atomicCasWord16Addr#
  casA (coerce → m) i x0 x1 s = case casA m i (cast @I2 x0) (cast @I2 x1) s of (# s', x #) → (# s', cast @U2 x #)
instance Eq_Atomic U4 where
  casP = coerce atomicCasWord32Addr#
  casA (coerce → m) i x0 x1 s = case casA m i (cast @I4 x0) (cast @I4 x1) s of (# s', x #) → (# s', cast @U4 x #)
instance Eq_Atomic U8 where
  casP = coerce atomicCasWord64Addr#
  casA (coerce → m) i x0 x1 s = case casA m i (cast @I8 x0) (cast @I8 x1) s of (# s', x #) → (# s', cast @U8 x #)
instance Eq_Atomic I where
  casA = coerce casIntArray#
  casP (coerce → p) x0 x1 s = case casP p (cast @U x0) (cast @U x1) s of (# s', x #) → (# s', cast @I x #)
instance Eq_Atomic I1 where
  casP (coerce → p) x0 x1 s = case casP p (cast @U1 x0) (cast @U1 x1) s of (# s', x #) → (# s', cast @I1 x #)
  casA = coerce casInt8Array#
instance Eq_Atomic I2 where
  casP (coerce → p) x0 x1 s = case casP p (cast @U2 x0) (cast @U2 x1) s of (# s', x #) → (# s', cast @I2 x #)
  casA = coerce casInt16Array#
instance Eq_Atomic I4 where
  casP (coerce → p) x0 x1 s = case casP p (cast @U4 x0) (cast @U4 x1) s of (# s', x #) → (# s', cast @I4 x #)
  casA = coerce casInt32Array#
instance Eq_Atomic I8 where
  casP (coerce → p) x0 x1 s = case casP p (cast @U8 x0) (cast @U8 x1) s of (# s', x #) → (# s', cast @I8 x #)
  casA = coerce casInt64Array#
instance Eq_Atomic Addr# where
  casP = coerce atomicCasAddrAddr#
  casA (coerce → m) i (cast → x0) (cast → x1) s = case casIntArray# m i x0 x1 s of (# s', (cast → x) #) → (# s', x #)
