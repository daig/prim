--------------------------------------------------------------------
-- | Description : Bitwise Operations on Signed Integers
--
-- Rarely sensible - prefer unsigned types for bit manipulation.
--------------------------------------------------------------------
{-# language CPP #-}
module Prim.I32.Bits where
import Prim.I32
#include "MachDeps.h"

{-# DEPRECATED shiftR#, shiftR, shiftL#, shiftL, shiftRL#, shiftRL "Signed logical bitwise operations are rarely sensible, prefer 'U32' instead" #-}

shiftR#, shiftR, shiftL#, shiftL, shiftRL#, shiftRL ∷ I32 → I → I32

-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftR# = uncheckedShiftRAInt32#

-- |Shift right arithmetic.  Result 0 or -1 (depending on sign)
-- if shift amount is not in the range 0 to word size - 1 inclusive.
shiftR x i = case i ≥ 32# of
  T → case x < 0# of {T → -1#; F → 0#}
  F → uncheckedShiftRAInt32# x i


-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# = uncheckedShiftLInt32#

-- | Shift left.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL x i = case i ≥ 32# of {T → 0#; F → uncheckedShiftLInt32# x i}


-- | Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# = uncheckedShiftRLInt32#
-- | Shift right logical.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL x i = case i ≥ 32# of {T → 0#; F → uncheckedShiftRLInt32# x i}
