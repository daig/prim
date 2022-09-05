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
  write# (Bytes_M ma) = writeA# ma ;\
  set (Bytes_M m@(M_UnpinnedByteArray# ma)) x s = case getSizeofMutableByteArray# ma s of (# s', n #) -> setB# m 0# n x s' }
INST_IDX_UNBOX(I)
INST_IDX_UNBOX(I8)
INST_IDX_UNBOX(I16)
INST_IDX_UNBOX(I32)
INST_IDX_UNBOX(I64)
INST_IDX_UNBOX(U)
INST_IDX_UNBOX(U8)
INST_IDX_UNBOX(U16)
INST_IDX_UNBOX(U32)
INST_IDX_UNBOX(U64)
INST_IDX_UNBOX(Char)
INST_IDX_UNBOX(Char8)
INST_IDX_UNBOX(P#)
-- INST_IDX_UNBOX((P_Stable s))

instance (♭) a ⇒ (a ∷ T r) ∈ Bytes where
  index# = indexB#
  read# = readB#
  write# = writeB#
--  set = setB#

deriving via Bytes instance (♭) a ⇒ (a ∷ T r) ∈ Bytes_Pinned
-- deriving via (A_Unbox (a :: T r)) instance (♭) a ⇒ (a ∷ T r) ∈ (A_Unbox_Pinned a)

instance (♭) x ⇒ (x ∷ T r) ∈ P# where
  index# = indexP#
  read# = coerce (readP# @x)
  write# = coerce (writeP# @x)
