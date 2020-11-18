module Ref.F32 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → F32
index# = indexFloatOffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST# s F32
read# = readFloatOffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → F32 → ST_# s
write# = writeFloatOffAddr#
