{-# language CPP #-}
module A.Index where
import Prim
import Do as Prim


type (∈) :: forall {r :: RuntimeRep} {r' :: RuntimeRep}. T r -> T r' -> Constraint
class (x ∷ T r) ∈ (a ∷ T r') where
  index# ∷ a → I {- ^ Offset in elements -} → x
  read# ∷ M a s → I → ST s x
  write# ∷ M a s → I → x → ST_ s
  -- | Set all elements
class x ∈ a => MemSet (x ∷ T r)  (a ∷ T r') where
  set ∷ M a s → x → ST_ s

-- | "A.Boxed.Small".
--
-- @index#@ Forces the indexing but not the value. For more laziness use 'indexLazy#'
--
-- @new@ uses sharing
instance x ∈ (A_Box_Small x) where
  write# = coerce (writeSmallArray# @_ @x)
  read# = coerce (readSmallArray# @_ @x)
  index# a i = case coerce (indexSmallArray# @x) a i of (# a #) -> a

-- | "A.P"
{-
instance (♭) a ⇒ (a ∷ T_ r) ∈ P a where
  index# (P# p) = indexP# p
  read# (P# p) = readP# p
  write# (P# p) = writeP# p
-}

-- | "A.Prim"
#define INST_IDX_UNBOX(A)\
instance A ∈ (A_Unbox A) where {\
  index# (Bytes a) = indexA# a ;\
  read# (Bytes_M ma) = readA# ma ;\
  write# (Bytes_M ma) = writeA# ma}

#define INST_MEMSET_UNBOX(A)\
instance MemSet A (A_Unbox A) where {\
  set (Bytes_M m@(M_UnpinnedByteArray# ma)) x s = case getSizeofMutableByteArray# ma s of (# s', n #) -> setB# m 0# n x s' }

#define INST_UNBOX(A)\
INST_IDX_UNBOX(A);\
INST_MEMSET_UNBOX(A)

INST_UNBOX(I)
INST_UNBOX(I8)
INST_UNBOX(I16)
INST_UNBOX(I32)
INST_UNBOX(I64)
INST_UNBOX(U)
INST_UNBOX(U8)
INST_UNBOX(U16)
INST_UNBOX(U32)
INST_UNBOX(U64)
INST_UNBOX(Char)
INST_UNBOX(Char8)
INST_UNBOX(P#)
-- INST_UNBOX((P_Stable s))

instance (♭) a ⇒ (a ∷ T r) ∈ Bytes where
  index# = indexB#
  read# = readB#
  write# = writeB#

deriving via Bytes instance (♭) a ⇒ (a ∷ T r) ∈ Bytes_Pinned
-- deriving via (A_Unbox (a :: T r)) instance (♭) a ⇒ (a ∷ T r) ∈ (A_Unbox_Pinned a)

instance (♭) x ⇒ (x ∷ T r) ∈ P# where
  index# = indexP#
  read# = coerce (readP# @x)
  write# = coerce (writeP# @x)
