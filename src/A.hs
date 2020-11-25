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
       → I -- ^ :ta
       → ST_# s

class Shrink (a ∷ T_A) where shrink ∷ M a s → I → ST_# s
