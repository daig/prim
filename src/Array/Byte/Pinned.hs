module Array.Byte.Pinned where
import Prelude hiding (Array)
import Array.Byte hiding (new)

new :: I64 -> ST s (Mutable s)
new = newPinnedByteArray#

-- TODO: add docs for which arg is which
newAligned :: I64 -> I64 -> ST s (Mutable s)
newAligned = newAlignedPinnedByteArray#

pinned' :: Array -> B
pinned' = isByteArrayPinned#

pinnedM' :: Mutable s -> B
pinnedM' = isMutableByteArrayPinned#

contents :: Array -> Addr
contents = byteArrayContents#
