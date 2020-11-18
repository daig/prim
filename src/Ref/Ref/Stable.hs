module Ref.Ref.Stable where
import qualified Ref
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → Ref.Stable a
index# = indexStablePtrOffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST# s (Ref.Stable a)
read# = readStablePtrOffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → Ref.Stable a → ST_# s
write# = writeStablePtrOffAddr#
