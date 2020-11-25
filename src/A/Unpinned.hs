module A.Unpinned where
import A

-- | Create a new uninitialized unpinned mutable byte array of specified size (in bytes),
-- in the specified state thread.
new ∷ I {-^ size in bytes -} → ST# s (MA s)
new = newByteArray#

resize ∷ MA s → I → ST# s (MA s)
resize = resizeMutableByteArray#
