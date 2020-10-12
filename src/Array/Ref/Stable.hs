module Array.Ref.Stable where
import Array.Byte
import qualified Ref

index# ∷ A → I {- ^ Offset in elments -} → Ref.Stable a
index# = indexStablePtrArray#

index## ∷ A → I {- ^ Offset in bytes -} → Ref.Stable a
index## = indexWord8ArrayAsStablePtr#

read# ∷ M s → I → ST s (Ref.Stable a)
read# = readStablePtrArray#

write# ∷ M s → I → Ref.Stable a → ST_ s
write# = writeStablePtrArray#
