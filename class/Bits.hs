{-# language CPP #-}
module Bits where
import Cmp
import Cast

#include "MachDeps.h"

-- | (Bitwise) logical operations on primitive values
type Logic ∷ ∀ {r ∷ Rep}. T r → Constraint
class Logic (a ∷ T r) where
  (∧), (∨), (⊕) ∷ a → a → a
  (¬) ∷ a → a
-- | Bit shuffling operations
type Bits ∷ ∀ {r ∷ Rep}. T r → Constraint
class Bits a where
  -- | Shift left.  Result undefined if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  shiftL# ∷ a → U → a
  -- | Shift left.  Result 0 if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  shiftL ∷ a → U → a
  -- |Shift right logical.  Result undefined if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  shiftR# ∷ a → U → a
  -- |Shift right logical.  Result 0 if shift amount is not
  --           in the range 0 to @size - 1@ inclusive.
  shiftR ∷ a → U → a
  -- |Shift left logical.  Accepts negative offset for right shifts.
  -- Result 0 if shift amount is not in the range @1 - size@ to @size - 1@ inclusive.
  shift ∷ a → I → a 
  -- | Count the number of set bits
  popCnt ∷ a → U
  -- | Count the number of leading zeroes
  clz ∷ a → U
  -- | Count the number of trailing zeroes
  ctz ∷ a → U
  -- | Swap the byte order
  byteSwap ∷ a → a
  -- | Reverse the order of the bits.
  bitReverse ∷ a → a
  pdep, pext ∷ a {-^ source -} → a {-^ mask -} → a
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


infixl 3 ∧
infixl 2 ⊕
infixl 1 ∨

-- | Boolean Operations
instance Logic B where
  (∧) = coerce andI#
  (∨) = coerce orI#
  (⊕) = coerce xorI#
  (¬) = (T ⊕)

instance Logic U where
  (∧) = and#
  (∨) = or#
  (⊕) = xor#
  (¬) = not#
instance Bits U where
  shiftL# w i = uncheckedShiftL# w (cast i)
  shiftL w i = case i ≥ WORD_SIZE_IN_BITS## of {B# 1# → 0##; B# 0# → shiftL# w i}
  shiftR# w i = coerce uncheckedShiftRL# w (cast @I i)
  shiftR w i = case i ≥ WORD_SIZE_IN_BITS## of {B# 1# → 0##; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt = popCnt#
  clz = clz#
  ctz = ctz#
  byteSwap = byteSwap#
  bitReverse = bitReverse#
  pdep = pdep#
  pext = pext#
  casP = atomicCasWordAddr#
  casA m i x0 x1 s = case casA m i (cast @I x0) (cast @I x1) s of (# s', x #) -> (# s', cast @U x #)

instance Logic U8 where
  (∧) = andWord8#
  (∨) = orWord8#
  (⊕) = xorWord8#
  (¬) = notWord8#
instance Bits U8 where
  shiftL# w i = uncheckedShiftLWord8# w (cast @I i)
  shiftL w i = case i ≥ 8## of {B# 1# → cast 0##; B# 0# → shiftL# w i}
  shiftR# w i = uncheckedShiftRLWord8# w (cast @I i)
  shiftR w i = case i ≥ 8## of {B# 1# → cast 0##; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt w = popCnt8# (cast @U w)
  clz w = clz8# (cast @U w)
  ctz w = ctz8# (cast @U w)
  byteSwap x = x
  bitReverse w = cast @U8 (bitReverse8# (cast @U w))
  pdep s m = cast @U8 (pdep8# (cast @U s) (cast @U m))
  pext s m = cast @U8 (pext8# (cast @U s) (cast @U m))
  casP = atomicCasWord8Addr#
  casA m i x0 x1 s = case casA m i (cast @I8 x0) (cast @I8 x1) s of (# s', x #) -> (# s', cast @U8 x #)

instance Logic U16 where
  (∧) = andWord16#
  (∨) = orWord16#
  (⊕) = xorWord16#
  (¬) = notWord16#
instance Bits U16 where
  shiftL# w i = uncheckedShiftLWord16# w (cast @I i)
  shiftL w i = case i ≥ 16## of {B# 1# → cast 0##; B# 0# → shiftL# w i}
  shiftR# w i = uncheckedShiftRLWord16# w (cast @I i)
  shiftR w i = case i ≥ 16## of {B# 1# → cast 0##; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt w = popCnt16# (cast @U w)
  clz w = clz16# (cast @U w)
  ctz w = ctz16# (cast @U w)
  byteSwap w = cast (byteSwap16# (cast w))
  bitReverse w = cast @U16 (bitReverse16# (cast @U w))
  pdep s m = cast @U16 (pdep16# (cast @U s) (cast @U m))
  pext s m = cast @U16 (pext16# (cast @U s) (cast @U m))
  casP = atomicCasWord16Addr#
  casA m i x0 x1 s = case casA m i (cast @I16 x0) (cast @I16 x1) s of (# s', x #) -> (# s', cast @U16 x #)

instance Logic U32 where
  (∧) = andWord32#
  (∨) = orWord32#
  (⊕) = xorWord32#
  (¬) = notWord32#
instance Bits U32 where
  shiftL# w i = uncheckedShiftLWord32# w (cast @I i)
  shiftL w i = case i ≥ 32## of {B# 1# → cast 0##; B# 0# → shiftL# w i}
  shiftR# w i = uncheckedShiftRLWord32# w (cast @I i)
  shiftR w i = case i ≥ 32## of {B# 1# → cast 0##; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt w = popCnt32# (cast @U w)
  clz w = clz32# (cast @U w)
  ctz w = ctz32# (cast @U w)
  byteSwap w = cast (byteSwap32# (cast w))
  bitReverse w = cast @U32 (bitReverse32# (cast @U w))
  pdep s m = cast @U32 (pdep32# (cast @U s) (cast @U m))
  pext s m = cast @U32 (pext32# (cast @U s) (cast @U m))
  casP = atomicCasWord32Addr#
  casA m i x0 x1 s = case casA m i (cast @I32 x0) (cast @I32 x1) s of (# s', x #) -> (# s', cast @U32 x #)

instance Logic U64 where
  (∧) = and64#
  (∨) = or64#
  (⊕) = xor64#
  (¬) = not64#
instance Bits U64 where
  shiftL# w i = uncheckedShiftL64# w (cast @I i)
  shiftL w i = case i ≥ 64## of {B# 1# → cast 0##; B# 0# → shiftL# w i}
  shiftR# w i = uncheckedShiftRL64# w (cast @I i)
  shiftR w i = case i ≥ 64## of {B# 1# → cast 0##; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt = popCnt64#
  clz = clz64#
  ctz = ctz64#
  byteSwap = byteSwap64#
  bitReverse = bitReverse64#
  pdep = pdep64#
  pext = pext64#
  casP = atomicCasWord64Addr#
  casA m i x0 x1 s = case casA m i (cast @I64 x0) (cast @I64 x1) s of (# s', x #) -> (# s', cast @U64 x #)

instance Logic I where
  (∧) = andI#
  (∨) = orI#
  (⊕) = xorI#
  (¬) = notI#
instance Bits I where
  shiftL# w i = uncheckedIShiftL# w (cast @I i)
  shiftL w i = case i ≥ WORD_SIZE_IN_BITS## of {B# 1# → cast 0##; B# 0# → shiftL# w i}
  shiftR# w i = uncheckedIShiftRA# w (cast @I i)
  shiftR w i = case i ≥ WORD_SIZE_IN_BITS## of {B# 1# → cast 0##; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt i = popCnt# (cast i)
  clz i = clz# (cast i)
  ctz i = ctz# (cast i)
  byteSwap i = cast (byteSwap# (cast i))
  bitReverse i = cast (bitReverse# (cast i))
  pdep i j = cast (pdep# (cast i) (cast j))
  pext i j = cast (pext# (cast i) (cast j))
  casA = coerce casIntArray#
  casP p x0 x1 s = case casP p (cast @U x0) (cast @U x1) s of (# s', x #) -> (# s', cast @I x #)

instance Logic I8 where
  a ∧ b = cast (andWord8# (cast a) (cast b))
  a ∨ b = cast (orWord8# (cast a) (cast b))
  a ⊕ b = cast (xorWord8# (cast a) (cast b))
  (¬) a = cast (notWord8# (cast a))
instance Bits I8 where
  shiftL# w i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  shiftL w i = case i ≥ 8## of {B# 1# → cast 0#; B# 0# → shiftL# w i}
  shiftR# w i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  shiftR w i = case i ≥ 8## of {B# 1# → cast 0#; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt i = popCnt8# (cast i)
  clz i = clz8# (cast i)
  ctz i = ctz8# (cast i)
  byteSwap i = i
  bitReverse i = cast (cast @U8 (bitReverse8# (cast i)))
  pdep i j = cast (cast @U8 (pdep8# (cast i) (cast j)))
  pext i j = cast (cast @U8 (pext8# (cast i) (cast j)))
  casP p x0 x1 s = case casP p (cast @U8 x0) (cast @U8 x1) s of (# s', x #) -> (# s', cast @I8 x #)
  casA = coerce casInt8Array#

instance Logic I16 where
  a ∧ b = cast (andWord16# (cast a) (cast b))
  a ∨ b = cast (orWord16# (cast a) (cast b))
  a ⊕ b = cast (xorWord16# (cast a) (cast b))
  (¬) a = cast (notWord16# (cast a))
instance Bits I16 where
  shiftL# w i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  shiftL w i = case i ≥ 16## of {B# 1# → cast 0#; B# 0# → shiftL# w i}
  shiftR# w i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  shiftR w i = case i ≥ 16## of {B# 1# → cast 0#; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt i = popCnt16# (cast i)
  clz i = clz16# (cast i)
  ctz i = ctz16# (cast i)
  byteSwap i = i
  bitReverse i = cast (cast @U16 (bitReverse16# (cast i)))
  pdep i j = cast (cast @U16 (pdep16# (cast i) (cast j)))
  pext i j = cast (cast @U16 (pext16# (cast i) (cast j)))
  casP p x0 x1 s = case casP p (cast @U16 x0) (cast @U16 x1) s of (# s', x #) -> (# s', cast @I16 x #)
  casA = coerce casInt16Array#

instance Logic I32 where
  a ∧ b = cast (andWord32# (cast a) (cast b))
  a ∨ b = cast (orWord32# (cast a) (cast b))
  a ⊕ b = cast (xorWord32# (cast a) (cast b))
  (¬) a = cast (notWord32# (cast a))
instance Bits I32 where
  shiftL# w i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  shiftL w i = case i ≥ 32## of {B# 1# → cast 0#; B# 0# → shiftL# w i}
  shiftR# w i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  shiftR w i = case i ≥ 32## of {B# 1# → cast 0#; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt i = popCnt32# (cast i)
  clz i = clz32# (cast i)
  ctz i = ctz32# (cast i)
  byteSwap i = i
  bitReverse i = cast (cast @U32 (bitReverse32# (cast i)))
  pdep i j = cast (cast @U32 (pdep32# (cast i) (cast j)))
  pext i j = cast (cast @U32 (pext32# (cast i) (cast j)))
  casP p x0 x1 s = case casP p (cast @U32 x0) (cast @U32 x1) s of (# s', x #) -> (# s', cast @I32 x #)
  casA = coerce casInt32Array#

instance Logic I64 where
  a ∧ b = cast (and64# (cast a) (cast b))
  a ∨ b = cast (or64# (cast a) (cast b))
  a ⊕ b = cast (xor64# (cast a) (cast b))
  (¬) a = cast (not64# (cast a))
instance Bits I64 where
  shiftL# w i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  shiftL w i = case i ≥ 64## of {B# 1# → cast 0#; B# 0# → shiftL# w i}
  shiftR# w i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  shiftR w i = case i ≥ 64## of {B# 1# → cast 0#; B# 0# → shiftR# w i}
  shift w i = case i ≥ 0# of {T → shiftL w (cast @U i); F → shiftR w (cast @U (negateInt# i))}
  popCnt i = popCnt64# (cast i)
  clz i = clz64# (cast i)
  ctz i = ctz64# (cast i)
  byteSwap i = i
  bitReverse i = cast (bitReverse64# (cast i))
  pdep i j = cast (pdep64# (cast i) (cast j))
  pext i j = cast (pext64# (cast i) (cast j))
  casP p x0 x1 s = case casP p (cast @U64 x0) (cast @U64 x1) s of (# s', x #) -> (# s', cast @I64 x #)
  casA = coerce casInt64Array#
