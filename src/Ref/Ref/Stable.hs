module Ref.Ref.Stable where
import qualified Ref.Stable as Stable
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → Stable.Ref a
index# = indexStablePtrOffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST# s (Stable.Ref a)
read# = readStablePtrOffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → Stable.Ref a → ST_# s
write# = writeStablePtrOffAddr#
