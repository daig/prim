--------------------------------------------------------------------
-- | Description : Boxed arrays
--------------------------------------------------------------------
module A.Box (A_Box,A_Box_M
               -- * instance reexports
               ,module X
               ) where
import Array as X (Array(..))
import Array.Index as X (type (∈)(..))
import Array.Copy as X (Copy(..))
import Array.Shrink as X (Shrink(..))
import Array.Atomic as X
