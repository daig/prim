{-# language CPP #-}
module Bits where
import Cmp
import Cast

#include "MachDeps.h"

-- | (Bitwise) logical operations on primitive values
type Logic ∷ ∀ {r}. T r → Constraint
class Logic a where
  (&&), (||), xor ∷ a → a → a
  not ∷ a → a
-- | Bit shuffling operations
type Bits ∷ ∀ {r}. T r → Constraint
class Bits a where
  -- | Shift left.  Result undefined if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  (<<#) ∷ a → U → a
  -- | Shift left.  Result 0 if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  (<<) ∷ a → U → a
  -- |Shift right.  Result undefined if shift amount is not
  --           in the range 0 to word @size - 1@ inclusive.
  (>>#) ∷ a → U → a
  -- |Shift right.  Result 0 if shift amount is not
  --           in the range 0 to @size - 1@ inclusive.
  (>>) ∷ a → U → a
  -- |Shift left logical.  Accepts negative offset for right shifts.
  -- Result 0 if shift amount is not in the range @1 - size@ to @size - 1@ inclusive.
  shift ∷ a → I → a 
  -- | Set the nth bit
  bit ∷ U → a
  -- | Check if the nth bit is set
  bit' ∷ a → U → B#
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

infixl 3 &&
infixl 2 `xor`
infixl 1 ||

infixl 8 <<#, <<, >>, >>#, `shift`

-- | Boolean Operations
instance Logic B# where
  (&&) = coerce andI#
  (||) = coerce orI#
  xor = coerce xorI#
  not = (T# `xor`)

instance Logic U where
  (&&) = and#
  (||) = or#
  xor = xor#
  not = not#
instance Bits U where
  w <<# i = uncheckedShiftL# w (cast i)
  w >># i = coerce uncheckedShiftRL# w (cast @I i)
  w << i = if i >= WORD_SIZE_IN_BITS## then 0## else w <<# i
  w >> i = if i >= WORD_SIZE_IN_BITS## then 0## else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (1## <<#)
  bit' x i = cast (bit i && x)
  popCnt = popCnt#
  clz = clz#
  ctz = ctz#
  byteSwap = byteSwap#
  bitReverse = bitReverse#
  pdep = pdep#
  pext = pext#

instance Logic U8 where
  (&&) = andWord8#
  (||) = orWord8#
  xor = xorWord8#
  not = notWord8#
instance Bits U8 where
  w <<# i = uncheckedShiftLWord8# w (cast @I i)
  w >># i = uncheckedShiftRLWord8# w (cast @I i)
  w << i = if i >= 8## then cast 0## else w <<# i
  w >> i = if i >= 8## then cast 0## else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1## <<#)
  bit' x i = cast (bit i && x)
  popCnt w = popCnt8# (cast w)
  clz w = clz8# (cast w)
  ctz w = ctz8# (cast w)
  byteSwap x = x
  bitReverse w = cast @U8 (bitReverse8# (cast w))
  pdep s m = cast @U8 (pdep8# (cast s) (cast m))
  pext s m = cast @U8 (pext8# (cast s) (cast m))

instance Logic U16 where
  (&&) = andWord16#
  (||) = orWord16#
  xor = xorWord16#
  not = notWord16#
instance Bits U16 where
  w <<# i = uncheckedShiftLWord16# w (cast @I i)
  w >># i = uncheckedShiftRLWord16# w (cast @I i)
  w << i = if i >= 16## then cast 0## else w <<# i
  w >> i = if i >= 16## then cast 0## else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1## <<#)
  bit' x i = cast (bit i && x)
  popCnt w = popCnt16# (cast w)
  clz w = clz16# (cast w)
  ctz w = ctz16# (cast w)
  byteSwap w = cast (byteSwap16# (cast w))
  bitReverse w = cast @U16 (bitReverse16# (cast w))
  pdep s m = cast @U16 (pdep16# (cast s) (cast m))
  pext s m = cast @U16 (pext16# (cast s) (cast m))

instance Logic U32 where
  (&&) = andWord32#
  (||) = orWord32#
  xor = xorWord32#
  not = notWord32#
instance Bits U32 where
  w <<# i = uncheckedShiftLWord32# w (cast @I i)
  w >># i = uncheckedShiftRLWord32# w (cast @I i)
  w << i = if i >= 32## then cast 0## else w <<# i
  w >> i = if i >= 32## then cast 0## else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1## <<#)
  bit' x i = cast (bit i && x)
  popCnt w = popCnt32# (cast w)
  clz w = clz32# (cast w)
  ctz w = ctz32# (cast w)
  byteSwap w = cast (byteSwap32# (cast w))
  bitReverse w = cast @U32 (bitReverse32# (cast w))
  pdep s m = cast @U32 (pdep32# (cast s) (cast m))
  pext s m = cast @U32 (pext32# (cast s) (cast m))

instance Logic U64 where
  (&&) = and64#
  (||) = or64#
  xor = xor64#
  not = not64#
instance Bits U64 where
  w <<# i = uncheckedShiftL64# w (cast @I i)
  w >># i = uncheckedShiftRL64# w (cast @I i)
  w << i = if i >= 64## then cast 0## else w <<# i
  w >> i = if i >= 64## then cast 0## else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1## <<#)
  bit' x i = cast (bit i && x)
  popCnt = popCnt64#
  clz = clz64#
  ctz = ctz64#
  byteSwap = byteSwap64#
  bitReverse = bitReverse64#
  pdep = pdep64#
  pext = pext64#

instance Logic I where
  (&&) = andI#
  (||) = orI#
  xor = xorI#
  not = notI#
instance Bits I where
  w <<# i = uncheckedIShiftL# w (cast @I i)
  w >># i = uncheckedIShiftRA# w (cast @I i)
  w << i = if i >= WORD_SIZE_IN_BITS## then cast 0## else w <<# i
  w >> i = if i >= WORD_SIZE_IN_BITS## then cast 0## else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (1# <<#)
  bit' x i = cast (bit i && x)
  popCnt i = popCnt# (cast i)
  clz i = clz# (cast i)
  ctz i = ctz# (cast i)
  byteSwap i = cast (byteSwap# (cast i))
  bitReverse i = cast (bitReverse# (cast i))
  pdep i j = cast (pdep# (cast i) (cast j))
  pext i j = cast (pext# (cast i) (cast j))

instance Logic I8 where
  a && b = cast (andWord8# (cast a) (cast b))
  a || b = cast (orWord8# (cast a) (cast b))
  a `xor` b = cast (xorWord8# (cast a) (cast b))
  not a = cast (notWord8# (cast a))
instance Bits I8 where
  w <<# i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  w >># i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  w << i = if i >= 8## then cast 0# else w <<# i
  w >> i = if i >= 8## then cast 0# else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1# <<#)
  bit' x i = cast (bit i && x)
  popCnt i = popCnt8# (cast i)
  clz i = clz8# (cast i)
  ctz i = ctz8# (cast i)
  byteSwap i = i
  bitReverse i = cast (cast @U8 (bitReverse8# (cast i)))
  pdep i j = cast (cast @U8 (pdep8# (cast i) (cast j)))
  pext i j = cast (cast @U8 (pext8# (cast i) (cast j)))

instance Logic I16 where
  a && b = cast (andWord16# (cast a) (cast b))
  a || b = cast (orWord16# (cast a) (cast b))
  a `xor` b = cast (xorWord16# (cast a) (cast b))
  not a = cast (notWord16# (cast a))
instance Bits I16 where
  w <<# i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  w >># i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  w << i = if i >= 16## then cast 0# else w <<# i
  w >> i = if i >= 16## then cast 0# else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1# <<#)
  bit' x i = cast (bit i && x)
  popCnt i = popCnt16# (cast i)
  clz i = clz16# (cast i)
  ctz i = ctz16# (cast i)
  byteSwap i = i
  bitReverse i = cast (cast @U16 (bitReverse16# (cast i)))
  pdep i j = cast (cast @U16 (pdep16# (cast i) (cast j)))
  pext i j = cast (cast @U16 (pext16# (cast i) (cast j)))

instance Logic I32 where
  a && b = cast (andWord32# (cast a) (cast b))
  a || b = cast (orWord32# (cast a) (cast b))
  a `xor` b = cast (xorWord32# (cast a) (cast b))
  not a = cast (notWord32# (cast a))
instance Bits I32 where
  w <<# i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  w >># i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  w << i = if i >= 32## then cast 0# else w <<# i
  w >> i = if i >= 32## then cast 0# else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1# <<#)
  bit' x i = cast (bit i && x)
  popCnt i = popCnt32# (cast i)
  clz i = clz32# (cast i)
  ctz i = ctz32# (cast i)
  byteSwap i = i
  bitReverse i = cast (cast @U32 (bitReverse32# (cast i)))
  pdep i j = cast (cast @U32 (pdep32# (cast i) (cast j)))
  pext i j = cast (cast @U32 (pext32# (cast i) (cast j)))

instance Logic I64 where
  a && b = cast (and64# (cast a) (cast b))
  a || b = cast (or64# (cast a) (cast b))
  a `xor` b = cast (xor64# (cast a) (cast b))
  not a = cast (not64# (cast a))
instance Bits I64 where
  w <<# i = cast (uncheckedIShiftL# (cast @I w) (cast @I i))
  w >># i = cast (uncheckedIShiftRA# (cast w) (cast @I i))
  w << i = if i >= 64## then cast 0# else w <<# i
  w >> i = if i >= 64## then cast 0# else w >># i
  shift w i = if i >= 0# then w << cast i else w >> cast (negateInt# i)
  bit = (cast 1# <<#)
  bit' x i = cast (bit i && x)
  popCnt i = popCnt64# (cast i)
  clz i = clz64# (cast i)
  ctz i = ctz64# (cast i)
  byteSwap i = i
  bitReverse i = cast (bitReverse64# (cast i))
  pdep i j = cast (pdep64# (cast i) (cast j))
  pext i j = cast (pext64# (cast i) (cast j))
