module Array.Byte.Pinned where
import qualified P.Byte as Byte
import Array.Byte hiding (new)

new ∷ I → ST# s (MA s)
new = newPinnedByteArray#

-- TODO: add docs for which arg is which
newAligned ∷ I → I → ST# s (MA s)
newAligned = newAlignedPinnedByteArray#

pinned' ∷ A → B#
pinned' = isByteArrayPinned#

pinnedMA' ∷ MA s → B#
pinnedMA' = isMutableByteArrayPinned#

contents ∷ A → Byte.P
contents = byteArrayContents#
