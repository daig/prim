--------------------------------------------------------------------
-- | Description : Typeclasses for Array operations
--------------------------------------------------------------------
{-# language TypeFamilyDependencies, FlexibleInstances,InstanceSigs,MultiParamTypeClasses #-}
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


class New# (a ∷ T_ r) where new# ∷ I {-^ size in elements -} → ST# s (M a s)
class Shrink (a ∷ T_A) where shrink ∷ M a s → I → ST_# s

-- | Size in elements. Uninitialized entries point recursively to the array itself.
instance New# ArrayArray# where new# = newArrayArray#

type family M (a ∷ k) (s ∷ T) = (ma ∷ k) | ma → a where
  M (Array# x) s = MutableArray# s x
  M (SmallArray# x) s = SmallMutableArray# s x
  M ByteArray# s = MutableByteArray# s
  M ArrayArray# s = MutableArrayArray# s
  M P s = P

class Size (a ∷ T_A) where
  -- | Number of elements
  size ∷ a → I
-- | # of bytes

-- | Make a mutable array immutable, without copying.
class Freeze## (a ∷ T_ r) where freeze## ∷ M a s → ST# s a
-- | Make an immutable array mutable, without copying.
class Thaw## (a ∷ T_ r) where thaw## ∷ a → ST# s (M a s)
class Thaw# (a ∷ T_ r) where
  thaw# ∷  a
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M a s)

-- | Create a new immutable array from a mutable by copying
class Freeze# (a ∷ T_ r) where
  freeze# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s a

class Copy (src ∷ T_ r) (dst ∷ T_ r') (s ∷ T) where
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
  index## ∷ a → I {- ^ Offset in bytes -} → x
  read# ∷ M a s → I → ST# s x
  write# ∷ M a s → I → x → ST_# s
  -- | Initialize an array
  new ∷ I {-^ size in elements -} → x → ST# s (M a s)
