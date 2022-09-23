{-# language InstanceSigs #-}
module Array.Copy where
import Prim
import Action

type Copy ∷ ∀ {rx} {rsrc} {rdst}. (T rx → T rsrc) → (T rx → T rdst) → ★ → Constraint
class Copy src dst s where
  -- | Copy the elements from the source to the destination.
  -- Both must fully contain the specified ranges and not overlap in memory,
  -- but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  copy ∷ ∀ x. dst ∋ x ⇒ src x → dst x → ST_ s

{-

instance Copy Array# (MutableArray# s) s where copy = copyArray# @_ @s
instance Copy (MutableArray# s) (MutableArray# s) s where copy = copyMutableArray#
instance Copy SmallArray# (SmallMutableArray# s) s where copy = copySmallArray#
instance Copy (SmallMutableArray# s) (SmallMutableArray# s) s where copy = copySmallMutableArray#

instance Copy UnboxedArray# (UnboxedMutableArray# s) s where
  copy ∷ ∀ x. UnboxedMutableArray# s ∋ x ⇒ UnboxedArray# x → I → UnboxedMutableArray# s x → I → I → ST_ s
  copy src (size @x → i) dst (size @x → j) (size @x → l) = coerce copyByteArray# src i dst j l
instance Copy PinnedArray# (PinnedMutableArray# s) s where
  copy ∷ ∀ x. PinnedMutableArray# s ∋ x ⇒ PinnedArray# x → I → PinnedMutableArray# s x → I → I → ST_ s
  copy src (size @x → i) dst (size @x → j) (size @x → l) = coerce copyByteArray# src i dst j l
instance Copy UnboxedArray# (ForeignMutableArray# s) s where
  copy ∷ ∀ x. ForeignMutableArray# s ∋ x ⇒ UnboxedArray# x → I → ForeignMutableArray# s x → I → I → ST_ s
  copy src (size @x → i) (coerce @_ @Addr# → dst) (size @x → j) (size @x → l) = coerce copyByteArrayToAddr# src i (dst +. j) l
instance Copy PinnedArray# (ForeignMutableArray# s) s where
  copy ∷ ∀ x. ForeignMutableArray# s ∋ x ⇒ PinnedArray# x → I → ForeignMutableArray# s x → I → I → ST_ s
  copy src (size @x → i) (coerce @_ @Addr# → dst) (size @x → j) (size @x → l) = coerce copyByteArrayToAddr# src i (dst +. j) l
instance Copy (UnboxedMutableArray# s) (ForeignMutableArray# s) s where
  copy ∷ ∀ x. ForeignMutableArray# s ∋ x ⇒ UnboxedMutableArray# s x → I → ForeignMutableArray# s x → I → I → ST_ s
  copy src (size @x → i) (coerce @_ @Addr# → dst) (size @x → j) (size @x → l) = coerce copyMutableByteArrayToAddr# src i (dst +. j) l
instance Copy ForeignArray# (UnboxedMutableArray# s) s where
  copy ∷ ∀ x. UnboxedMutableArray# s ∋ x ⇒ ForeignArray# x → I → UnboxedMutableArray# s x → I → I → ST_ s
  copy (coerce @_ @Addr# → src) (size @x → i) dst (size @x → j) (size @x → l) = coerce copyAddrToByteArray# (src +. i) dst j l
instance Copy (ForeignMutableArray# s) (UnboxedMutableArray# s) s where
  copy ∷ ∀ x. UnboxedMutableArray# s ∋ x ⇒ ForeignMutableArray# s x → I → UnboxedMutableArray# s x → I → I → ST_ s
  copy (coerce @_ @Addr# → src) (size @x → i) dst (size @x → j) (size @x → l) = coerce copyAddrToByteArray# (src +. i) dst j l
instance Copy ForeignArray# (PinnedMutableArray# s) s where
  copy ∷ ∀ x. PinnedMutableArray# s ∋ x ⇒ ForeignArray# x → I → PinnedMutableArray# s x → I → I → ST_ s
  copy (coerce @_ @Addr# → src) (size @x → i) dst (size @x → j) (size @x → l) = coerce copyAddrToByteArray# (src +. i) dst j l
instance Copy (ForeignMutableArray# s) (PinnedMutableArray# s) s where
  copy ∷ ∀ x. PinnedMutableArray# s ∋ x ⇒ ForeignMutableArray# s x → I → PinnedMutableArray# s x → I → I → ST_ s
  copy (coerce @_ @Addr# → src) (size @x → i) dst (size @x → j) (size @x → l) = coerce copyAddrToByteArray# (src +. i) dst j l
  -}
