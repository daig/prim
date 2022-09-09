{-# language CPP #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Bits where
import Cmp
import Cast

#include "MachDeps.h"

-- | Bitwise algebriac operations on primitive values
class 𝔹 (a ∷ T r) where
  (∧), (∨), (⊕) ∷ a → a → a
  (¬) ∷ a → a
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

infixl 3 ∧
infixl 2 ⊕
infixl 1 ∨

-- | Boolean Operations
instance 𝔹 B where
  (∧) = coerce andI#
  (∨) = coerce orI#
  (⊕) = coerce xorI#
  (¬) = (T ⊕)
  shiftL# (B# x) i = T ∧ (B# do uncheckedIShiftL# x (cast i))
  shiftL x = \case {0## → x; _ → F}
  shiftR# (B# x) i =  T ∧ (B# do uncheckedIShiftRL# x (cast i))
  shiftR = shiftL
  shift x = \case {0# → x; _ → F}
  popCnt (B# 0#) = 0##
  popCnt (B# 1#) = 1##
  clz (B# 0#) = 1##
  clz (B# 1#) = 0##
  ctz (B# 1#) = 0##
  ctz (B# 0#) = 0##
  byteSwap x = x
  bitReverse x = x
  pdep = (∧); pext = (∧)

instance 𝔹 U where
  (∧) = and#
  (∨) = or#
  (⊕) = xor#
  (¬) = not#
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

instance 𝔹 U8 where
  (∧) = andWord8#
  (∨) = orWord8#
  (⊕) = xorWord8#
  (¬) = notWord8#
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

instance 𝔹 U16 where
  (∧) = andWord16#
  (∨) = orWord16#
  (⊕) = xorWord16#
  (¬) = notWord16#
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

instance 𝔹 U32 where
  (∧) = andWord32#
  (∨) = orWord32#
  (⊕) = xorWord32#
  (¬) = notWord32#
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

instance 𝔹 U64 where
  (∧) = and64#
  (∨) = or64#
  (⊕) = xor64#
  (¬) = not64#
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
