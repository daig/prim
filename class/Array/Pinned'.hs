module Array.Pinned' where

type Pinned' ∷ T_ → Constraint
class Pinned' a where
  -- | Determine whether the array is gaurenteed not to move during GC.
  -- This happens when it's allocated above a certain size (3kb), for GC performance reasons.
  -- If you want gaurenteed pinning (eg for FFI), use 'A_Box_Pinned'.
  pinned' ∷ a → B#
instance Pinned' ByteArray# where pinned' = coerce isByteArrayPinned#
deriving via ByteArray# instance Pinned' (UnboxedArray# x)

instance Pinned' (MutableByteArray# s) where pinned' = coerce isMutableByteArrayPinned#
deriving via (MutableByteArray# s) instance Pinned' (UnboxedMutableArray# s x)

instance Pinned' (PinnedMutableArray# s x) where pinned' _ = T#
