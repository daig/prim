module Array.Stable where
import Array.Byte
import qualified P.Stable as Stable

index# ∷ A → I {- ^ Offset in elments -} → Stable.P a
index# = indexStablePtrArray#

index## ∷ A → I {- ^ Offset in bytes -} → Stable.P a
index## = indexWord8ArrayAsStablePtr#

read# ∷ MA s → I → ST# s (Stable.P a)
read# = readStablePtrArray#

write# ∷ MA s → I → Stable.P a → ST_# s
write# = writeStablePtrArray#
