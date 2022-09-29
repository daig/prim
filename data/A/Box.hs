--------------------------------------------------------------------
-- | Description : Boxed Arrays
--------------------------------------------------------------------
module A.Box (Array#, MutableArray#
               -- * misc utilities
               ,module A.Box
               -- * instance reexports
               ,module X
               ) where
import Array as X (Array(..))
import Array.Index as X (type (∈#)(..))
import Array.Copy as X (Copy(..))
