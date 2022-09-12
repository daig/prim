--------------------------------------------------------------------
-- | Description : Unboxed Arrays of Primitive Types
--------------------------------------------------------------------
module A.Unbox (A_Unbox,A_Unbox_M
               -- * misc utilities
               ,module A.Unbox
               -- * instance reexports
               ,module X
               ) where
import Array as X (Array(..))
import Array.Index as X (type (∈)(..))
import Array.Copy as X (Copy(..))
import Array.Shrink as X (Shrink(..))
import Prim.Atomic as X (Atomic(..),Eq_Atomic(..),Num_Atomic(..),Logic_Atomic(..))

resize :: A_Unbox_M x s -> I -> ST s (A_Unbox_M x s)
resize = coerce resizeMutableByteArray#
