--------------------------------------------------------------------
-- | Description : Unboxed Arrays of Primitive Types
--------------------------------------------------------------------
module A.Unbox (A', A
               -- * misc utilities
               ,module A.Unbox
               -- * instance reexports
               ,module X
               ) where
import Array as X (Array(..))
import Array.Index as X (Index(..))
import Array.Copy as X (Copy(..))
import Array.Shrink as X (Shrink(..))
import Array.Pinned' as X (Pinned'(..))
import Prim.Atomic as X (Atomic(..),Eq_Atomic(..),Num_Atomic(..),Logic_Atomic(..))

resize ∷ ∀ {r} s (x ∷ T r). A s x → I → ST s (A s x)
resize = coerce resizeMutableByteArray#
