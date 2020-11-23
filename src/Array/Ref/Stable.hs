module Array.Ref.Stable where
import Array.Byte
import qualified Ref.Stable as Stable

index# ∷ A → I {- ^ Offset in elments -} → Stable.Ref a
index# = indexStablePtrArray#

index## ∷ A → I {- ^ Offset in bytes -} → Stable.Ref a
index## = indexWord8ArrayAsStablePtr#

read# ∷ MA s → I → ST# s (Stable.Ref a)
read# = readStablePtrArray#

write# ∷ MA s → I → Stable.Ref a → ST_# s
write# = writeStablePtrArray#
