--------------------------------------------------------------------
-- | Description : Primitive Array types
--------------------------------------------------------------------
{-# language TypeFamilyDependencies, FlexibleInstances,InstanceSigs,MultiParamTypeClasses #-}
{-# language CPP #-}
{-# language QuantifiedConstraints #-}
{-# language ForeignFunctionInterface, UnliftedFFITypes #-}
module Prim.A (Bytes,MBytes,Refs,MRefs,module X, module Prim.A)where
import P hiding (Prim)
import Prim.Char
import Prim.A.Prim.Elts
import Prim.I32 (I32(..))
import Prim.I16 (I16(..))
import Prim.I8 (I8(..))
import Prim.I64 (I64(..))
import Prim.I
import qualified P.Stable as Stable
import {-# source #-} qualified Prim.A.Prim as Prim
import {-# source #-} qualified Prim.A.Prim.Pinned as Pinned
import {-# source #-} qualified Prim.A.Array as Ref
import {-# source #-} qualified Prim.A.Boxed.Big as Big
import {-# source #-} qualified Prim.A.Boxed as Boxed
import qualified P.STM as STM
import Stock.Int
#include "MachDeps.h"
#include "HsBaseConfig.h"
import GHC.Types (IO(..))
import Prim.A.M as X

type Bytes = ByteArray#
type MBytes = MutableByteArray#
type Refs = ArrayArray#
type MRefs = MutableArrayArray#

class 𝔸 (a ∷ T_A) where
  -- | Uninitialized array.
  new# ∷ I {-^ size in elements -} → ST s (M a s)
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s → ST s a
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a → ST s (M a s)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  a
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s a
  -- | Number of elements
  len ∷ a → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ M a s → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ M a s → ST s I
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  clone# ∷ a
         → I -- ^ Source offset
         → I -- ^ number of elements to copy
         → a
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  cloneM# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s)
class 𝔸 a ⇒ Shrink (a ∷ T_A) where shrink ∷ M a s → I → ST_ s


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
       → ST_ s

class (x ∷ T_ r) ∈ (a ∷ T_ r') where
  index# ∷ a → I {- ^ Offset in elements -} → x
  read# ∷ M a s → I → ST s x
  write# ∷ M a s → I → x → ST_ s
  -- | Set all elements
  set ∷ M a s → x → ST_ s
  -- | Initialize an array
  new ∷ I {-^ size in elements -} → x → ST s (M a s)

-- | "A.P"
instance (♭) a ⇒ (a ∷ T_ r) ∈ P a where
  index# (P# p) = indexP# p
  read# (P# p) = readP# p
  write# (P# p) = writeP# p
