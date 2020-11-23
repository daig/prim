module P.P.Stable where
import qualified P.Stable as Stable
import P.Byte

index# ∷ P → I {- ^ Offset in elments -} → Stable.P a
index# = indexStablePtrOffAddr#

read# ∷ P → I {- ^ Offset in elements -} → ST# s (Stable.P a)
read# = readStablePtrOffAddr#

write# ∷ P → I {- ^ Offset in elements -} → Stable.P a → ST_# s
write# = writeStablePtrOffAddr#
