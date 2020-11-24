module A.Pinned where
import P
import A hiding (new)

new ∷ I → ST# s (MA s)
new = newPinnedByteArray#

-- TODO: add docs for which arg is which
newAligned ∷ I → I → ST# s (MA s)
newAligned = newAlignedPinnedByteArray#

contents ∷ A → P
contents = byteArrayContents#
