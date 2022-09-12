--------------------------------------------------------------------
-- | Description : Raw un-garbage-collected address references
--------------------------------------------------------------------
module P (P#,Addr#
         -- * misc utilities
         ,module P
         -- * instance reexports
         ,module X
         ) where
import Cmp as X (type (≡)(..), type (≤)(..))
import Array.Copy as X (Copy(..))
import Array.Index as X (type (∈)(..))
import Prim as X (Prim(..))
import Prim.Atomic as X (Atomic(..),Eq_Atomic(..),Logic_Atomic(..),Num_Atomic(..))

-- | The distinguished null pointer. You probably want to use 'nullAddr#' or the `(≡)` instance directly.
pattern Null ∷ P#
pattern Null ← nullAddr# where Null = nullAddr#

-- |Advances the given address by the given offset (in bytes).
(∔) ∷ P# -> I -> P#
(∔) = plusAddr#

-- |Computes the offset (in bytes) required to get from the second to the first argument.
(߸) ∷ P# → P# → I
(߸) = minusAddr#

(.//) ∷ P# → I → I
(.//) = remAddr#
