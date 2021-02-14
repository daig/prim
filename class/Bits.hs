{-# language LinearTypes #-}
{-# language CPP #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Bits where
import Cmp
import Cast

#include "MachDeps.h"

-- | Bitwise algebriac operations on primitive values
class 𝔹 (a ∷ T r) where
  (∧), (∨), (⊕) ∷ a ⊸ a ⊸ a
  (¬) ∷ a ⊸ a
  -- | Shift left.  Result undefined if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  shiftL# ∷ a ⊸ U ⊸ a
  -- | Shift left.  Result 0 if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  shiftL ∷ a ⊸ U ⊸ a
  -- |Shift right logical.  Result undefined if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  shiftR# ∷ a ⊸ U ⊸ a
  -- |Shift right logical.  Result 0 if shift amount is not
  --           in the range 0 to @size - 1@ inclusive.
  shiftR ∷ a ⊸ U ⊸ a
  -- |Shift left logical.  Accepts negative offset for right shifts.
  -- Result 0 if shift amount is not in the range @1 - size@ to @size - 1@ inclusive.
  shift ∷ a ⊸ I ⊸ a 
  -- | Count the number of set bits
  popCnt ∷ a ⊸ U
  -- | Count the number of leading zeroes
  clz ∷ a ⊸ U
  -- | Count the number of trailing zeroes
  ctz ∷ a ⊸ U
  -- | Swap the byte order
  byteSwap ∷ a ⊸ a
  -- | Reverse the order of the bits.
  bitReverse ∷ a ⊸ a
  pdep, pext ∷ a ⊸ a ⊸ a

infixl 3 ∧
infixl 2 ⊕
infixl 1 ∨

-- | Boolean Operations
instance 𝔹 B where
  (∧) = coerce (λ\i → λ do andI# i)
  (∨) = coerce (λ\i → λ do orI# i)
  (⊕) = coerce (λ\i → λ do xorI# i)
  (¬) = (T ⊕)
  shiftL# (B# x) i = T ∧ (B# do (λ\a → λ\b → uncheckedIShiftL# a b) x (cast i))
  shiftL = λ\ x → λ\case {0## → x; _ → F}
  shiftR# (B# x) i =  T ∧ (B# do (λ\a → λ\b → uncheckedIShiftRL# a b) x (cast i))
  shiftR = shiftL
  shift = λ\ x → λ\case {0# → x; _ → F}
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
  (∧) = λ\i → λ do and# i
  (∨) = λ\i → λ do or# i
  (⊕) = λ\i → λ do xor# i
  (¬) = λ not#
  shiftL# = λ\w → λ\i → uncheckedShiftL# w (cast i)
  shiftL = λ\w → λ\i → case i ≥ WORD_SIZE_IN_BITS## of {B# 1# → 0##; B# 0# → shiftL# w i}
  shiftR# = λ\i → λ\w  → uncheckedShiftRL# w (cast i)
  shiftR = λ\w → λ\i → case i ≥ WORD_SIZE_IN_BITS## of {B# 1# → 0##; B# 0# → shiftL# w i}
  shift = λ\w → λ\i → case i ≥ 0# of
    T → case i ≥ WORD_SIZE_IN_BITS# of {B# 1# → 0##; B# 0# → uncheckedShiftL# w i}
    F → case i ≤ WORD_SIZE_IN_BITS# of {B# 1# → 0##; B# 0# → uncheckedShiftRL# w (negateInt# i)}
  popCnt = λ popCnt#
  clz = λ clz#
  ctz = λ ctz#
  byteSwap = λ byteSwap#
  bitReverse = λ bitReverse#
  pdep = λ\i → λ do pdep# i
  pext = λ\i → λ do pext# i

instance 𝔹 U8 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U8# u) = cast (u ¬)
  shiftL# (U8# w) i = cast do (λ\x → λ\y → uncheckedShiftL# x y) w (cast i)
  shiftL = λ\w → λ\i → case i ≥ 8## of {T → U8# 0##; F → shiftL# w i}
  shiftR# (U8# w) i = cast do (λ\x → λ\y → uncheckedShiftRL# x y) w (cast i)
  shiftR = λ\w → λ\i → case i ≥ 8## of {T → U8# 0##; F → shiftL# w i}
  shift = λ\(U8# w) → λ\i → case i ≥ 0# of
    T → case i ≥  8# of {T → U8# 0##; F → cast (uncheckedShiftRL# w i)}
    F → case i ≤ -8# of {T → U8# 0##; F → cast (uncheckedShiftRL# w (negateInt# i))}
  popCnt = coerce (λ popCnt8#)
  clz = coerce (λ clz8#)
  ctz = coerce (λ ctz8#)
  byteSwap x = x
  bitReverse = coerce (λ bitReverse8#)
  pdep = coerce do λ\i → λ do pdep8# i
  pext = coerce do λ\i → λ do pext8# i

instance 𝔹 U16 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U16# u) = cast (u ¬)
  shiftL# (U16# w) i = cast do (λ\x → λ\y → uncheckedShiftL# x y) w (cast i)
  shiftL = λ\w → λ\i → case i ≥ 16## of {T → U16# 0##; F → shiftL# w i}
  shiftR# (U16# w) i = cast do (λ\x → λ\y → uncheckedShiftRL# x y) w (cast i)
  shiftR = λ\w → λ\i → case i ≥ 16## of {T → U16# 0##; F → shiftL# w i}
  shift = λ\(U16# w) → λ\i → case i ≥ 0# of
    T → case i ≥  16# of {T → U16# 0##; F → cast (uncheckedShiftRL# w i)}
    F → case i ≤ -16# of {T → U16# 0##; F → cast (uncheckedShiftRL# w (negateInt# i))}
  popCnt = coerce (λ popCnt16#)
  clz = coerce (λ clz16#)
  ctz = coerce (λ ctz16#)
  byteSwap = coerce (λ byteSwap16#)
  bitReverse = coerce (λ bitReverse16#)
  pdep = coerce do λ\i → λ do pdep16# i
  pext = coerce do λ\i → λ do pext16# i

instance 𝔹 U32 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U32# u) = cast (u ¬)
  shiftL# (U32# w) i = cast do (λ\x → λ\y → uncheckedShiftL# x y) w (cast i)
  shiftL = λ\w → λ\i → case i ≥ 32## of {T → U32# 0##; F → shiftL# w i}
  shiftR# (U32# w) i = cast ((λ\x → λ\y →  uncheckedShiftRL# x y) w (cast i))
  shiftR = λ\w → λ\i → case i ≥ 32## of {T → U32# 0##; F → shiftL# w i}
  shift = λ\(U32# w) → λ\i → case i ≥ 0# of
    T → case i ≥  32# of {T → U32# 0##; F → cast (uncheckedShiftRL# w i)}
    F → case i ≤ -32# of {T → U32# 0##; F → cast (uncheckedShiftRL# w (negateInt# i))}
  popCnt = coerce (λ popCnt32#)
  clz = coerce (λ clz32#)
  ctz = coerce (λ ctz32#)
  byteSwap = coerce (λ byteSwap32#)
  bitReverse = coerce (λ bitReverse32#)
  pdep = coerce (λ\i → λ do pdep32# i)
  pext = coerce (λ\i → λ do pext32# i)

deriving newtype instance 𝔹 U64
