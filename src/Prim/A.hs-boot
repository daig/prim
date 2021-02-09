module Prim.A where
import {-# source #-} Prim.A.M

class 𝔸 (a ∷ T_A) where
  new# ∷ I {-^ size in elements -} → ST# s (M a s)
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s → ST# s a
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a → ST# s (M a s)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  a
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M a s)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s a
  -- | Number of elements
  len ∷ a → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ M a s → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ M a s → ST# s I
  clone# ∷ a → I → I → a
  cloneM# ∷ M a s → I → I → ST# s (M a s)

class 𝔸 a ⇒ Shrink (a ∷ T_A) where shrink ∷ M a s → I → ST_# s

class Copy (src ∷ T_ r) (dst ∷ T_ r') s where
  copy ∷ src → I → dst → I → I → ST_# s
