--------------------------------------------------------------------
-- | Description : Typeclasses for Array operations
--------------------------------------------------------------------
{-# language TypeFamilyDependencies, FlexibleInstances,InstanceSigs,MultiParamTypeClasses #-}
{-# language CPP #-}
module A where
import P hiding (Prim)
import Char8
import Char
import I32 (I32(..))
import I16 (I16(..))
import I8 (I8(..))
import I64 (I64(..))
import qualified P.Stable as Stable
import qualified B
import {-# source #-} qualified A.Unboxed as Unboxed
import Stock.Int
#include "MachDeps.h"
#include "HsBaseConfig.h"


class 𝔸 (a ∷ T_A) where
  new# ∷ I {-^ size in elements -} → ST# s (M a s)
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s → ST# s a
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a → ST# s (M a s)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  a
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M a s)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s a
  -- | Number of elements
  len ∷ a → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ M a s → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ M a s → ST# s I

class 𝔸 a ⇒ Shrink (a ∷ T_A) where shrink ∷ M a s → I → ST_# s

type family M (a ∷ k) (s ∷ T) = (ma ∷ k) | ma → a where
  M (Array# x) s = MutableArray# s x
  M (SmallArray# x) s = SmallMutableArray# s x
  M ByteArray# s = MutableByteArray# s
  M ArrayArray# s = MutableArrayArray# s
  M P s = P
  M (Unboxed.A (x ∷ T_ r)) s = Unboxed.MA s x

class Copy (src ∷ T_ r) (dst ∷ T_ r') s where
  -- | Copy the elements from the source to the destination.
  -- Both must fully contain the specified ranges and not overlap in memory,
  -- but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  copy ∷ src
       → I -- ^ Source Offset (bytes)
       → dst
       → I -- ^ Destination Offset (bytes)
       → I -- ^ Number of elements to copy
       → ST_# s

class (x ∷ T_ r) ∈ (a ∷ T_ r') where
  index# ∷ a → I {- ^ Offset in elements -} → x
  read# ∷ M a s → I → ST# s x
  write# ∷ M a s → I → x → ST_# s
  -- | Initialize an array
  new ∷ I {-^ size in elements -} → x → ST# s (M a s)

