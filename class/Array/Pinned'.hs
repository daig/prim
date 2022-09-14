module Array.Pinned' where

type Pinned' ∷ T_ → Constraint
class Pinned' a where
  -- | Determine whether the array is gaurenteed not to move during GC.
  -- This happens when it's allocated above a certain size (3kb), for GC performance reasons.
  -- If you want gaurenteed pinning (eg for FFI), use 'A_Box_Pinned'.
  pinned' ∷ a → B#
instance Pinned' Bytes where pinned' = coerce isByteArrayPinned#
instance Pinned' (Bytes_M s) where pinned' = coerce isMutableByteArrayPinned#
instance Pinned' Bytes_Pinned where pinned' _ = T#
instance Pinned' (Bytes_Pinned_M s) where pinned' _ = T#
deriving via Bytes instance Pinned' (A_Unbox x)
deriving via (Bytes_M s) instance Pinned' (A_Unbox_M x s)
deriving via Bytes_Pinned instance Pinned' (A_Unbox_Pinned x)
deriving via (Bytes_Pinned_M s) instance Pinned' (A_Unbox_Pinned_M x s)
