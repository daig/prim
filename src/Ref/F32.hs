module Ref.F32 where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → F32
index# = indexFloatOffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST s F32
read# = readFloatOffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → F32 → ST_ s
write# = writeFloatOffAddr#
