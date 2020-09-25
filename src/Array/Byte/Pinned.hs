module Array.Byte.Pinned where
import qualified Ref
import Array.Byte hiding (new)

new ∷ I64 → ST s (M s)
new = newPinnedByteArray#

-- TODO: add docs for which arg is which
newAligned ∷ I64 → I64 → ST s (M s)
newAligned = newAlignedPinnedByteArray#

pinned' ∷ A → B
pinned' = isByteArrayPinned#

pinnedM' ∷ M s → B
pinnedM' = isMutableByteArrayPinned#

contents ∷ A → Ref.Byte
contents = byteArrayContents#
