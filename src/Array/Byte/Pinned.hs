module Array.Byte.Pinned where
import qualified Ref
import Array.Byte hiding (new)

new ∷ I64 → ST s (M s)
new = newPinnedByteArray#

-- TODO: add docs for which arg is which
newAligned ∷ I64 → I64 → ST s (M s)
newAligned = newAlignedPinnedByteArray#

pinned' ∷ A → I1
pinned' = isByteArrayPinned#

pinnedM' ∷ M s → I1
pinnedM' = isMutableByteArrayPinned#

contents ∷ A → Ref.Byte
contents = byteArrayContents#
