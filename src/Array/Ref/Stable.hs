module Array.Ref.Stable where
import Array.Byte
import qualified Ref

index# ∷ A
       → I64 -- Offset in elments
       → Ref.Stable a
index# = indexStablePtrArray#

index## ∷ A
        → I64 -- Offset in bytes
        → Ref.Stable a
index## = indexWord8ArrayAsStablePtr#

read# ∷ M s → I64 → ST s (Ref.Stable a)
read# = readStablePtrArray#

write# ∷ M s → I64 → Ref.Stable a → ST_ s
write# = writeStablePtrArray#
