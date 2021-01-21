--------------------------------------------------------------------
-- | Description : Raw unmanaged pointers
--------------------------------------------------------------------
{-# language TypeSynonymInstances,UnliftedNewtypes, GADTs, TypeOperators #-}
module P (P#,module P) where
import Char
import Char8
import I32 (I32(..))
import I16 (I16(..))
import I64 (I64(..))
import I8 (I8(..))
import qualified P.Stable as Stable


-- | hack to expose nullAddr#
pattern Null ∷ P#
pattern Null ← nullAddr# where Null = nullAddr#

-- |Advances the given address by the given offset in bytes.
(∔) ∷ I → P# → P#
i ∔ a = plusAddr# a i

-- |Computes the offset required to get from the second to the first argument.
(߸) ∷ P# → P# → I
(߸) = minusAddr#

(.//) ∷ P# → I → I
(.//) = remAddr#

pattern Addr# ∷ I → P#
pattern Addr# i ← (addr2Int# → i) where Addr# = int2Addr#
{-# DEPRECATED Addr# "This pattern is strongly deprecated" #-}

-- | Immutable raw pointer to a valid memory region containing sum number of @x@
newtype P (x ∷ T_ r) ∷ T_P where P# ∷ ∀ r (x ∷ T_ r). P# → P x

instance (≤) P# where (>) = coerce gtAddr# ; (≥) = coerce geAddr# ; (<) = coerce ltAddr# ; (≤) = coerce leAddr# ; 
instance (≡) P# where (≡) = coerce eqAddr# ; (≠) = coerce neAddr#

