--------------------------------------------------------------------
-- | Description : Unboxed Arrays of Primitive Types
--------------------------------------------------------------------
module A.Unbox.Pinned (PinnedArray#,PinnedMutableArray#
               -- * misc utilities
               ,module A.Unbox.Pinned
               -- * instance reexports
               ,module X
               ) where
import Array as X (Array(..))
import Array.Index as X (Index(..))
import Array.Copy as X (Copy(..))
import Array.Shrink as X (Shrink(..))
import Array.Pinned' as X (Pinned'(..))
import Prim.Atomic as X (Atomic(..),Eq_Atomic(..),Num_Atomic(..),Logic_Atomic(..))

aligned ∷ I {- ^ size in bytes -} → I {- ^ alignment in bytes (must be power of 2) -} → ST s (PinnedMutableArray# s x)
aligned = coerce newAlignedPinnedByteArray#
