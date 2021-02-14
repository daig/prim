--------------------------------------------------------------------
-- | Description : Bitwise Operations on Signed Integers
--
-- Rarely sensible - prefer unsigned types for bit manipulation.
--------------------------------------------------------------------
{-# language CPP #-}
module Prim.I.Bits where
import Prim.I
#include "MachDeps.h"

{-# DEPRECATED shiftL#, shiftL, shiftRL#, shiftRL, (∧), (∨), (⊕) "Signed logical bitwise operations are rarely sensible, prefer 'U' instead" #-}

-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftR# ∷ I → I → I
shiftR# = uncheckedIShiftRA#

-- |Shift right arithmetic.  Result 0 or -1 (depending on sign)
-- if shift amount is not in the range 0 to word size - 1 inclusive.
shiftR ∷ I → I → I
shiftR i x = case i ≥ WORD_SIZE_IN_BITS# of
  T → case x < 0# of {T → -1#; F → 0#}
  F → uncheckedIShiftRA# x i


shiftL#, shiftL, shiftRL#, shiftRL ∷ I → I → I
-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# i x = uncheckedIShiftL# x i

-- | Shift left.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL i x = case i ≥ WORD_SIZE_IN_BITS# of {T → 0#; F → uncheckedIShiftL# x i}


-- | Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# i x = uncheckedIShiftRL# x i
-- | Shift right logical.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL i x = case i ≥ WORD_SIZE_IN_BITS# of {T → 0#; F → uncheckedIShiftRL# x i}

-- | /Warning/: Bitwise operations rarely make sense on signed ints,
-- Consider using 'U' instead.
--
-- @(n ¬) = -n - 1@
(∧) = andI#; (∨) = orI#; (⊕) = xorI#; (¬) = notI
